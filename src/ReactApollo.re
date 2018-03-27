open ApolloClient;

open ApolloTypes;

[@bs.module] external gql : ApolloTypes.gql = "graphql-tag";

let unwrap_bool = (b: option(bool)) =>
  switch (b) {
  | Some(value) => Js.Nullable.return(Js.Boolean.to_js_boolean(value))
  | None => Js.Nullable.undefined
  };

module type Config = {let query: string; type t; let parse: Js.Json.t => t;};

module CreateQuery = (Config: Config) => {
  let queryGql = gql(. Config.query);
  type apolloOptions = {
    .
    "query": ApolloTypes.queryString,
    "variables": Js.Json.t,
  };
  type apolloData = {
    .
    "loading": Js.boolean,
    "data": Js.Nullable.t(Js.Json.t),
    "error": Js.Nullable.t(apolloError),
    "refetch":
      [@bs.meth] (
        Js.Null_undefined.t(Js.Json.t) => Js.Promise.t(apolloData)
      ),
    "networkStatus": int,
    "variables": Js.Null_undefined.t(Js.Json.t),
    "fetchMore": [@bs.meth] (apolloOptions => Js.Promise.t(unit)),
  };
  type data =
    | Loading
    | LoadingWithData(Config.t)
    | Error(apolloError)
    | Data(Config.t)
    | NoData;
  type apollo = {
    data,
    refetch: option(Js.Json.t) => Js.Promise.t(data),
    fetchMore: (~variables: Js.Json.t) => Js.Promise.t(unit),
    networkStatus: int,
  };
  let apolloDataToReason: apolloData => data =
    apolloData =>
      switch (
        Js.to_bool(apolloData##loading),
        Js.Null_undefined.toOption(apolloData##data),
        Js.Null_undefined.toOption(apolloData##error),
      ) {
      | (true, None, _) => Loading
      | (true, Some(data), _) =>
        switch (Config.parse(data)) {
        | parsedData => LoadingWithData(parsedData)
        | exception _ => Loading
        }
      | (false, Some(data), None) => Data(Config.parse(data))
      | (false, _, Some(error)) => Error(error)
      | (false, None, None) => NoData
      };
  [@bs.module "react-apollo"]
  external reactClass : ReasonReact.reactClass = "Query";
  let convertJsInputToReason = (apolloData: apolloData) => {
    data: apolloDataToReason(apolloData),
    refetch: variables =>
      apolloData##refetch(variables |> Js.Null_undefined.fromOption)
      |> Js.Promise.then_(data =>
           data |> apolloDataToReason |> Js.Promise.resolve
         ),
    fetchMore: (~variables) =>
      apolloData##fetchMore({"variables": variables, "query": queryGql}),
    networkStatus: apolloData##networkStatus,
  };
  let make =
      (
        ~fetchPolicy: option(string)=?,
        ~variables: option(Js.Json.t)=?,
        ~ssr: option(bool)=?,
        ~notifyOnNetworkStatusChange: option(bool)=?,
        ~pollInterval: option(int)=?,
        children: apollo => ReasonReact.reactElement,
      ) =>
    ReasonReact.wrapJsForReason(
      ~reactClass,
      ~props=
        Js.Nullable.(
          {
            "query": queryGql,
            "variables": variables |> Js.Null_undefined.fromOption,
            "fetchPolicy": fromOption(fetchPolicy),
            "ssr": unwrap_bool(ssr),
            "notifyOnNetworkStatusChange":
              unwrap_bool(notifyOnNetworkStatusChange),
            "pollInterval": fromOption(pollInterval),
          }
        ),
      apolloData =>
      apolloData |> convertJsInputToReason |> children
    );
};

module CreateMutation = (Config: Config) => {
  let queryGql = gql(. Config.query);
  type apolloOptions = {
    .
    "query": ApolloTypes.queryString,
    "variables": Js.Json.t,
  };
  type apolloData = {
    .
    "loading": Js.boolean,
    "called": Js.boolean,
    "data": Js.Nullable.t(Js.Json.t),
    "error": Js.Nullable.t(apolloError),
    "networkStatus": int,
    "variables": Js.Null_undefined.t(Js.Json.t),
  };
  type data =
    | Loading
    | LoadingWithData(Config.t)
    | Called
    | CalledWithData(Config.t)
    | Error(apolloError)
    | Data(Config.t)
    | NoData;
  type apollo = {
    data,
    networkStatus: int,
  };
  let apolloDataToReason: apolloData => data =
    apolloData =>
      switch (
        Js.to_bool(apolloData##called),
        Js.to_bool(apolloData##loading),
        Js.Null_undefined.toOption(apolloData##data),
        Js.Null_undefined.toOption(apolloData##error),
      ) {
      | (true, false, None, _) => Called
      | (true, _, Some(data), _) =>
        switch (Config.parse(data)) {
        | parsedData => CalledWithData(parsedData)
        | exception _ => Called
        }
      | (_, true, None, _) => Loading
      | (false, true, Some(data), _) =>
        switch (Config.parse(data)) {
        | parsedData => LoadingWithData(parsedData)
        | exception _ => Loading
        }
      | (false, false, Some(data), None) => Data(Config.parse(data))
      | (false, false, _, Some(error)) => Error(error)
      | (false, false, None, None) => NoData
      };
  [@bs.module "react-apollo"]
  external reactClass : ReasonReact.reactClass = "Mutation";
  let convertJsInputToReason = (apolloData: apolloData) => {
    data: apolloDataToReason(apolloData),
    networkStatus: apolloData##networkStatus,
  };
  type mutationOptions = {
    .
    "variables": Js.Nullable.t(Js.Json.t),
    "refetchQueries": Js.Nullable.t(array(string)),
  };
  type apolloMutation =
    Js.Nullable.t(mutationOptions) => Js.Promise.t(apolloData);
  let make =
      (
        ~variables: option(Js.Json.t)=?,
        ~onError: option(unit => unit)=?,
        ~onCompleted: option(unit => unit)=?,
        children: (apolloMutation, apollo) => ReasonReact.reactElement,
      ) =>
    ReasonReact.wrapJsForReason(
      ~reactClass,
      ~props=
        Js.Nullable.(
          {
            "query": queryGql,
            "variables": variables |> Js.Null_undefined.fromOption,
            "onError": fromOption(onError),
            "onCompleted": fromOption(onCompleted),
          }
        ),
      (mutation, apolloData) =>
      children(mutation, convertJsInputToReason(apolloData))
    );
};

module ApolloConsumer = {
  [@bs.module "react-apollo"]
  external reactClass : ReasonReact.reactClass = "ApolloConsumer";
  let make = (children: generatedApolloClient => ReasonReact.reactElement) =>
    ReasonReact.wrapJsForReason(~reactClass, ~props=Js.Obj.empty(), children);
};

module ApolloProvider = {
  [@bs.module "react-apollo"]
  external reactClass : ReasonReact.reactClass = "ApolloProvider";
  let make = (~client: generatedApolloClient, children) =>
    ReasonReact.wrapJsForReason(
      ~reactClass,
      ~props={"client": client},
      children,
    );
};