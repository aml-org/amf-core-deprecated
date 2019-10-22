exports.expander = class JsonLdExpander {
    // Expands jsonld if flattened and returns a native js object. If it is already expanded, it just parses and returns the js object.
    static expand(flattened) {
        const f = JSON.parse(flattened)
        const graph = f['@graph']
        if (graph) {
            const cache = {}
            JsonLdExpander.populateCache(graph, cache)
            for (let i = graph.length - 1; i >= 0; i--) {
                const current = graph[i]
                const id = current['@id']
                cache[id] = current
                JsonLdExpander.traverse(current, cache)
            }
            return JsonLdExpander.normalize(graph)
        } else return f
    }

    static traverse(element, cache) {
        Object.keys(element).forEach(key => {
            const value = element[key]
            if (typeof value === 'object' && value !== null && !key.endsWith("link-target") && !key.endsWith("reference-id") && !key.endsWith("fixPoint")) {
                const isArray = Array.isArray(value)
                if (isArray) {
                    if (this.isArrayOfLinks(value)) {
                        JsonLdExpander.replaceIdReferencesInArray(element, key, cache)
                    } else if (key !== "@type") {
                        element[key] = element[key].map(value => this.normalizeValue(value))
                    }

                } else {
                    JsonLdExpander.replaceIdReference(element, key, cache)
                }
            } else if((typeof value === 'string' || typeof value === 'number' || typeof value === 'boolean') && key !== '@id') {
                element[key] = [this.normalizeValue(value)]
            }
        })
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
    static normalize(graph) {
        return [graph[0]]
    }
    static replaceIdReference(element, key, cache) {
        const linkObject = element[key]
        const retrievedTarget = JsonLdExpander.retrieveIfIsLink(linkObject, cache)
        element[key] = [retrievedTarget]
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
        } else if(element['http://a.ml/vocabularies/document-source-maps#element'] && element['http://a.ml/vocabularies/document-source-maps#value']){
            // annotations expansion to @value
            const elementKey = 'http://a.ml/vocabularies/document-source-maps#element'
            const valueKey = 'http://a.ml/vocabularies/document-source-maps#value'
            element[elementKey] = [{'@value': element[elementKey]}]
            element[valueKey] = [{'@value': element[valueKey]}]
            return element
        } else {
            return element
        }
    }
    static isLinkObject(element) {
        return element['@id'] !== undefined && Object.keys(element).length === 1
    }

    static isArrayOfLinks(array) {
        return array.every(element => this.isLinkObject(element));
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