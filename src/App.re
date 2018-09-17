[%bs.raw {|require('./App.css')|}];

[@bs.module] external logo : string = "./logo.svg";

let component = ReasonReact.statelessComponent("App");

let make = (~state: Tetris.state, ~dispatch, _children) => {
  ...component,
  render: _self => <div className="App"> <Tetris data=state /> </div>,
};
