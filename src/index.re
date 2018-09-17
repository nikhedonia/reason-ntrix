[%bs.raw {|require('./index.css')|}];
[@bs.module "./registerServiceWorker"]


external register_service_worker : unit => unit = "default";

Reductive.Store.subscribe(Tetris.store, ()=>{
  let state = Reductive.Store.getState(Tetris.store);
  let dispatch = (action) => {
    Reductive.Store.dispatch(
      Tetris.store, action
    );
  };

  ReactDOMRe.renderToElementWithId(
    <App state=state dispatch=dispatch/>,
    "root",
  );
});

Reductive.Store.dispatch(Tetris.store, ())

let dispatch = action => {
  Reductive.Store.dispatch(Tetris.store, action);
}

Js.Global.setInterval(
  () => dispatch(Tetris.Tick),
  1000
)

KeyBoard.register( key => switch(key) {
 | "w" => dispatch(Tetris.Rotate)
 | "a" => dispatch(Tetris.MoveLeft)
 | "d" => dispatch(Tetris.MoveRight)
 | "s" => dispatch(Tetris.Tick)
 | x => Js.log(x)
})
