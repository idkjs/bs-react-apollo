let component = ReasonReact.statelessComponent("Page");

let int_from_opt = (~default: int=0, option: option(int)) =>
  switch (option) {
  | None => default
  | Some(a) => a
  };

module CountQuery = [%graphql
  {|
    query films {
      allFilms {
        totalCount
      }
    }
|}
];

module FilmsQuery = ReactApollo.CreateQuery(CountQuery);

let make = _children => {
  ...component,
  render: _self => {
    let countQuery = CountQuery.make();
    <FilmsQuery variables=countQuery##variables>
      ...(
           ({data}) =>
             <div>
               <h1> ("Star Wars 2: " |> ReasonReact.stringToElement) </h1>
               (
                 switch (data) {
                 | NoData => "No Data" |> ReasonReact.stringToElement
                 | Error(_) =>
                   "Something Went Wrong" |> ReasonReact.stringToElement
                 | Loading => "Loading" |> ReasonReact.stringToElement
                 | Data(result)
                 | LoadingWithData(result) =>
                   switch (result##allFilms) {
                   | None => "No Films" |> ReasonReact.stringToElement
                   | Some(films) =>
                     "Films Found: "
                     ++ (films##totalCount |> int_from_opt |> string_of_int)
                     |> ReasonReact.stringToElement
                   }
                 }
               )
             </div>
         )
    </FilmsQuery>;
  },
};