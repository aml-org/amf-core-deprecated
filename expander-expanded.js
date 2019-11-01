exports.expander = class JsonLdExpander {
    // Expands jsonld if flattened and returns a native js object. If it is already expanded, it just parses and returns the js object.
    static expand(flattened) {
        const f = JSON.parse(flattened)
        const graph = f['@graph']
        const context = f['@context']
        if (graph) {
            const cache = {}
            JsonLdExpander.populateCache(graph, cache)
            for (let i = graph.length - 1; i >= 0; i--) {
                const current = graph[i]
                const id = current['@id']
                cache[id] = current
                JsonLdExpander.traverse(current, cache, context)
            }
            return JsonLdExpander.normalize(graph, context)
        } else return f
    }

    static traverse(element, cache, context) {
        Object.keys(element).forEach(key => {
            const value = element[key]
            if (typeof value === 'object' && value !== null && !key.endsWith("link-target") && !key.endsWith("reference-id") && !key.endsWith("fixPoint")) {
                const isArray = Array.isArray(value)
                if (isArray) {
                    if (this.isArrayOfLinks(key, value)) {
                        JsonLdExpander.replaceIdReferencesInArray(element, key, cache)
                    } else if (key.endsWith('tracked-element')  || key.endsWith('parsed-json-schema')) {
                        let val = element[key]
                        const elementKey    = this.compactName('element', 'http://a.ml/vocabularies/document-source-maps#', context)
                        const valueKey      = this.compactName('value', 'http://a.ml/vocabularies/document-source-maps#', context)
                        val[0][elementKey]  = [this.normalizeValue(val[0][elementKey])]
                        val[0][valueKey]    = [this.normalizeValue(val[0][valueKey])]
                    } else if(key.endsWith('in') || key.endsWith('items')){ // shacl in
                        JsonLdExpander.traverse(element[key][0], cache, context)
                    } else if (key !== "@type" && !key.endsWith('customDomainProperties')) {
                        element[key] = element[key].map(value => this.normalizeValue(value))
                    }

                } else {
                    const wrapInArray = !this.keyIsCustomDomainProperty(key, element, context)
                    JsonLdExpander.replaceIdReference(element, key, cache, wrapInArray)
                }
            } else if((typeof value === 'string' || typeof value === 'number' || typeof value === 'boolean') && key !== '@id' && key !== '@type') {
                element[key] = [this.normalizeValue(value)]
            }
        })
    }

    static keyIsCustomDomainProperty(key, element, context) {
        const customProperties = Object.entries(element).filter(entry => entry[0].endsWith('customDomainProperties'))[0]
        if (customProperties) {
            return customProperties[1].filter(
                customProp =>
                    this.expandName(customProp['@id'], context) === key || customProp['@id'] === key
            ).length > 0
        } else {
            return false
        }

    }

    static expandName(key, context) {
        if (context !== undefined &&  context['@base'] !== undefined) {
            return context['@base'] + key
        } else {
            return key
        }
    }

    static compactName(key, prefix, context) {
        if (context) {
            const retrieved = Object.entries(context).filter(entry => entry[1] === prefix)[0]
            if (retrieved) {
                const term = retrieved[0]
                return term + ":" + key
            } else {
                return prefix + key
            }
        } else {
            return prefix + key
        }

    }

    static normalizeValue(value) {
        return {"@value": value}
    }

    static populateCache(graph, cache) {
        graph.forEach(element => {
            const id = element['@id']
            cache[id] = element
        })
    }
    static normalize(graph, context) {
        const baseUnit = graph.filter(x => x['@id'] === "./")[0]
        if (context) {
            baseUnit['@context'] = context

        }
        return [baseUnit]
    }
    static replaceIdReference(element, key, cache, wrapInArray) {
        const linkObject = element[key]
        const retrievedTarget = JsonLdExpander.retrieveIfIsLink(linkObject, cache)
        if (wrapInArray) {
            element[key] = [retrievedTarget]
        } else {
            element[key] = retrievedTarget
        }
    }
    static replaceIdReferencesInArray(element, key, cache) {
        const links = element[key]
        element[key] = links.map(linkObject => JsonLdExpander.retrieveIfIsLink(linkObject, cache))
    }
    static retrieveIfIsLink(element, cache) {
        if (JsonLdExpander.isLinkObject(element)) {
            const retrievedTarget = JsonLdExpander.retrieveLinkTargetFrom(element, cache)
            if (retrievedTarget) {
                return retrievedTarget
            } else {
                return element
            }
        } else {
            return element
        }
    }
    static isLinkObject(element) {
        return element['@id'] !== undefined && Object.keys(element).length === 1
    }

    static isArrayOfLinks(field, array) {
        return !field.endsWith('customDomainProperties') && array.every(element => this.isLinkObject(element))
    }

    static retrieveLinkTargetFrom(linkObject, cache) {
        const id = linkObject['@id']
        const target = cache[id]
        if (target === undefined) {
            console.log("cache miss " + id)
        }
        return target
    }
}