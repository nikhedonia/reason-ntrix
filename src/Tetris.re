open Array;
open ReactDOMRe;
open Pieces;
open LookupTables;
open Position;
open Board;

let component = ReasonReact.statelessComponent("Tetris");


type actions = 
  | MoveLeft
  | MoveRight
  | Rotate
  | Tick
  | Drop
  | GameOver
  | ()

type state = {
  board: array(array(int)),
  lines: int,
  lastAction: actions,
  piece: Pieces.oriented,
  pos: position
};



let makeMove = (s: state, step: position) => {
  if( !overlap(s.board, s.piece.g, addP(s.pos, step) ) ) {
   addP(s.pos, step) 
  } else {
    s.pos
  }
}

let moveDown = s => {
  ...s,
  lastAction: Tick,
  pos: makeMove(s, {x:0, y:+1}),
} 

let rotate = (s) => { 
  let rotated = rotateRight(s.piece.g);
  let next = transition(s.piece.o, R);
  let tests = wallKickTest(s.piece.g|>length, s.piece.o, next);
  let p = findOffset(s.board, rotated, s.pos, tests) 
  switch (p) {
    | None => {...s, lastAction: Rotate}
    | Some(offset) => {
      {
        ...s,
        lastAction: Rotate,
        piece: {g:rotated, o:next},
        pos: addP(s.pos, offset)
      }
    }  
  }
}

let stateReducer = (s: state, a: actions): state => {
  switch (s.lastAction, a) {
    | (GameOver, _) => s
    | (_, Rotate) => rotate(s)
    | (_, MoveLeft) => {
      ...s,
      lastAction: MoveLeft,
      pos: makeMove(s, {x:-1, y:0})
    }
    | (_, MoveRight) => {
      ...s,
      lastAction: MoveRight,
      pos: makeMove(s, {x:+1, y:0})
    }
    | (Tick|Drop, Tick) => {
      if (overlap(s.board, s.piece.g, addP(s.pos, {x:0, y:+1}))) {
        let (removed, board) = eliminate(
          merge(s.board, s.piece.g, s.pos)
        );
        
        let nextPiece = pieces[Random.int(pieces|>length)];
        let nextPos = {x:4, y:0};

        {
          board,
          lastAction: if(overlap(board, nextPiece.g, nextPos))
            GameOver
          else Tick,
          lines: s.lines+removed,
          piece: nextPiece,
          pos: nextPos
        }
      } else moveDown(s)
    }
    | (_, Tick) => moveDown(s)
    | (_,_) => s
  }
}




let store = Reductive.Store.create(
  ~reducer=stateReducer,
  ~preloadedState={
    piece: pieces[0],
    lastAction: (),
    lines: 0,
    pos: {x:4, y:0},
    board: createBoard(10, 20)
  },
());

let cellColor = cell => switch(cell) {
  | 0 => "#000"
  | 2 => "#222"
  | _ => "#fff"
}



let gridStyle = Style.make(
  ~borderCollapse= "collapse",
  ~emptyCells= "show",
  ~border= "solid 1px black",
());

let cellStyle = Style.make(
  ~background= "white",
  ~color="transparent",
  ~border= "solid 1px #333",
  ~fontSize="5",
  ~minWidth= "1vh",
  ~minHeight= "1vh",
());


let make = (~data, _children) => {
  ...component,
    render: _self =>
      <div>
      <table style={
        gridStyle
      }> <tbody>(
        ReasonReact.array(
         materialize(data.board, data.piece.g, data.pos) 
          |> mapi( (i, rows) => 
          <tr key=string_of_int(i)>( ReasonReact.array(
              rows |> mapi( (j, cell) =>
              <td key=string_of_int(j) style={Style.combine(
                  cellStyle,
                  Style.make(~background= cellColor(cell) ,())
              )}>(ReasonReact.string("+"))</td>
              )) )</tr>
          )
        )) </tbody> </table>
      <div> 
        <b>(ReasonReact.string(string_of_int(data.lines)))</b>
        (switch(data.lastAction){ 
          | GameOver => <b>(ReasonReact.string("You lost"))</b> 
          | _ => <b/> 
         })
      </div>
      </div>
}
