const fs = require('fs');
const flattened = fs.readFileSync('path', 'utf8'); // TODO fill me
expand(flattened);

function expand(flattened) {
    const f = JSON.parse(flattened);
    const graph = f['@graph'];
    const cache = {};

    populateCache(graph, cache);

    for (let i = graph.length - 1; i >= 0; i--) {
        const current = graph[i];
        const id = current['@id'];
        cache[id] = current;
        traverse(current, cache)
    }
    const result = normalize(graph);
    fs.writeFileSync("path", JSON.stringify(result, undefined, 2)) // TODO fill me
}

function traverse(element, cache) {
    Object.keys(element).forEach(key => {
        const value = element[key];
        if (typeof value === 'object' && value !== null && !key.endsWith("link-target") && !key.endsWith("fixPoint") && !key.endsWith("reference-id")) {
            const isArray = Array.isArray(value);
            if (isArray) {
                replaceIdReferencesInArray(element, key, cache)
            } else {
                replaceIdReference(element, key, cache)
            }
        }
    })
}

function populateCache(graph, cache) {
    graph.forEach(element => {
        const id = element['@id'];
        cache[id] = element
    })
}

function normalize(graph) {
    const baseUnit = graph.filter(x => x['@id'] === "./")[0]
    return [baseUnit];
}

function replaceIdReference(element, key, cache) {
    const linkObject = element[key];
    const retrievedTarget = retrieveIfIsLink(linkObject, cache);
    element[key] = [retrievedTarget]
}

function replaceIdReferencesInArray(element, key, cache) {
    const links = element[key];
    element[key] = links.map(linkObject => retrieveIfIsLink(linkObject, cache))
}

function retrieveIfIsLink(element, cache) {
    if (isLinkObject(element)) {
        const retrievedTarget = retrieveLinkTargetFrom(element, cache);
        if (retrievedTarget) {
            return retrievedTarget
        } else {
            return element
        }
    } else {
        return element
    }
}

function isLinkObject(element) {
    return element['@id'] !== undefined && Object.keys(element).length === 1
}

function retrieveLinkTargetFrom(linkObject, cache) {
    const id = linkObject['@id'];
    const target = cache[id];
    if (target === undefined) {
        console.log("cache miss " + id)
    }
    return target
}