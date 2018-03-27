open ApolloInMemoryCache;

type dataObject = {
  .
  "__typename": string,
  "id": string,
};

let inMemoryCache =
  createInMemoryCache(~dataIdFromObject=(obj: dataObject) => obj##id, ());

let httpLink = ApolloLinks.createHttpLink(~uri="http://swapi.apis.guru/", ());

let client =
  ApolloClient.createApolloClient({
    "link": httpLink,
    "cache": inMemoryCache,
    "ssrMode": Js.Nullable.undefined,
    "ssrForceFetchDelay": Js.Nullable.undefined,
    "connectToDevTools": Js.Nullable.undefined,
    "queryDeduplication": Js.Nullable.undefined,
  });