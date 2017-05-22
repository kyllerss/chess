var ChessModel = function () {

    var _gameId = null;
    var _rawState = {};
    var _board = {};
    var _validMoves = {};
    var _moveablePieces = {};
    var _pieceMapping = {p: "Pawn", r: "Rook", n: "Knight", b: "Bishop", q: "Queen", k: "King"};
    var _colorMapping = { w: "White"
                        , b: "Black"
                        , g: "Green"
                        , c: "Blue"
                        , p: "Purple"
                        , o: "Orange"
                        , y: "Yellow"
                        , r: "Red"
                        , n: "None"};
    var _VIEWURL = null;
    var _MOVEURL = null;
    var _playerTurn = null;

    function _move(pId, newCoord) {

        console.log("pId [" + pId + "] has moved to [" + newCoord[0] + ", " + newCoord[1] + "].");

        var rawPiece = _rawState.pieces[pId];
        var oldRow = rawPiece.xy[0];
        var oldCol = rawPiece.xy[1];
        var oldCoord = [oldRow, oldCol];

        var oldSpace = _fetchSpace(oldCoord);
        var newSpace = _fetchSpace(newCoord);

        // swap piece state
        newSpace.pieceType = oldSpace.pieceType;
        newSpace.pieceColor = oldSpace.pieceColor;
        newSpace.pieceId = oldSpace.pieceId;

        oldSpace.pieceType = null;
        oldSpace.pieceColor = null;
        oldSpace.pieceId = null;

        var moveState = {pId: pId,
                         newCoord: newCoord,
                         oldCoord: oldCoord};

        // TODO: show modal

        // Fetch server state and refresh
        var moveData = {
            pieceId: pId,
            coord: "[" + newCoord[0] + "," + newCoord[1] + "]"
        };

        $.ajax({
            url: _MOVEURL,
            dataType: "json",
            method: "POST",
            data: moveData,
            traditional: true
        })
        .done(function(data) {

            console.log("Received response move command: " + data);
            _updateState(data);

            // Notify of new board state.
            nerve.send({
                channel: 'board-update',
                context: moveState
            });
        })
        .fail(function( jqXHR, textStatus, errorThrown ) {
            alert(jqXHR.responseJSON.error);
        })
        .always(function(data) {

            // TODO: clear modal

        });
    };

    function _addValidMove(pId, move) {

        /*
             {1: {1: [1,2,3], 2: [4,3]},
              2: {1: [3,3], 3: [3]}}
         */

        var row = move[0];
        var col = move[1];

        var rowMap = _validMoves[row];
        if (rowMap == null) {
            rowMap = {};
            _validMoves[row] = rowMap;
        }

        var pIds = rowMap[col];
        if (pIds == null) {
            pIds = [];
            rowMap[col] = pIds;
        }

        pIds.push(pId);
    };

    function _calculateColor(color) {
        return _colorMapping[color];
    };

    function _calculatePieceType(type) {
        return _pieceMapping[type];
    };

    function _updateState(newState) {
        
        console.log("Received from server new state: " + newState);
            
        _gameId = newState.gameId;
        _playerTurn = newState.playerTurn;
        _rawState = newState;

        // layout initial board
        _board = [];
        for (var rowId = 0; rowId < _rawState.board.length; rowId++) {
            
            var rawRow = _rawState.board[rowId];
            
            var rowState = [];
            for (var colId = 0; colId < rawRow.length; colId++) {
                
                var color = _calculateColor(rawRow[colId]);
                var space = {color: color};
                
                rowState.push(space);
            }
            
            _board.push(rowState);
        }
        
        // place pieces
        var pId, row, col;
        for (pId in _rawState.pieces) {
            
            var rawPiece = _rawState.pieces[pId];
            
            var rowCoord = rawPiece.xy[0];
            var colCoord = rawPiece.xy[1];
            
            var spaceState = _board[rowCoord][colCoord];
            spaceState.pieceType = _calculatePieceType(rawPiece.t);
            spaceState.pieceColor = _calculateColor(rawPiece.c);
            spaceState.pieceId = pId;
        }
        
        // decompose valid moves
        var moves;
        _validMoves = {};
        for (pId in _rawState.moves) {
            
            moves = _rawState.moves[pId];
            for (var i = 0; i < moves.length; i++) {
                
                var move = moves[i];
                _addValidMove(pId, move);
            }
        }
        
        _moveablePieces = {};
        for (pId in _rawState.moves) {
            
            moves = _rawState.moves[pId];
            if (moves && moves.length > 0) {
                _moveablePieces[pId] = true;
            }
        }
        
        /*
         {
         // board layout
         board : [['w', 'b', 'w', 'b', 'w', 'b', 'w', 'b'],
         ['b', 'w', 'b', 'w', 'b', 'w', 'b', 'w'],
         ['w', 'b', 'w', 'b', 'w', 'b', 'w', 'b'],
         ['b', 'w', 'b', 'w', 'b', 'w', 'b', 'w'],
         ['w', 'b', 'w', 'b', 'w', 'b', 'w', 'b'],
         ['b', 'w', 'b', 'w', 'b', 'w', 'b', 'w'],
         ['w', 'b', 'w', 'b', 'w', 'b', 'w', 'b'],
         ['b', 'w', 'b', 'w', 'b', 'w', 'b', 'w']],
         
         // t -> type, c -> color, p -> player id
         pieces: {
         // player 1
         1: {t: 'r', c: 'b', p: 1, xy: [0,0]},
         2: {t: 'n', c: 'b', p: 1, xy: [0,1]},
         3: {t: 'b', c: 'b', p: 1, xy: [0,2]},
         4: {t: 'q', c: 'b', p: 1, xy: [0,3]},
         5: {t: 'k', c: 'b', p: 1, xy: [0,4]},
         6: {t: 'b', c: 'b', p: 1, xy: [0,5]},
         7: {t: 'k', c: 'b', p: 1, xy: [0,6]},
         8: {t: 'r', c: 'b', p: 1, xy: [0,7]},
         9: {t: 'p', c: 'b', p: 1, xy: [1,0]},
         10: {t: 'p', c: 'b', p: 1, xy: [1,1]},
         11: {t: 'p', c: 'b', p: 1, xy: [1,2]},
         12: {t: 'p', c: 'b', p: 1, xy: [1,3]},
         13: {t: 'p', c: 'b', p: 1, xy: [1,4]},
         14: {t: 'p', c: 'b', p: 1, xy: [1,5]},
         15: {t: 'p', c: 'b', p: 1, xy: [1,6]},
         16: {t: 'p', c: 'b', p: 1, xy: [1,7]},
         
         // player 2
         17: {t: 'p', c: 'w', p: 2, xy: [6,0]},
         18: {t: 'p', c: 'w', p: 2, xy: [6,1]},
         19: {t: 'p', c: 'w', p: 2, xy: [6,2]},
         20: {t: 'p', c: 'w', p: 2, xy: [6,3]},
         21: {t: 'p', c: 'w', p: 2, xy: [6,4]},
         22: {t: 'p', c: 'w', p: 2, xy: [6,5]},
         23: {t: 'p', c: 'w', p: 2, xy: [6,6]},
         24: {t: 'p', c: 'w', p: 2, xy: [6,7]},
         25: {t: 'r', c: 'w', p: 2, xy: [7,0]},
         26: {t: 'n', c: 'w', p: 2, xy: [7,1]},
         27: {t: 'b', c: 'w', p: 2, xy: [7,2]},
         28: {t: 'q', c: 'w', p: 2, xy: [7,3]},
         29: {t: 'k', c: 'w', p: 2, xy: [7,4]},
         30: {t: 'b', c: 'w', p: 2, xy: [7,5]},
         31: {t: 'n', c: 'w', p: 2, xy: [7,6]},
         32: {t: 'r', c: 'w', p: 2, xy: [7,7]}
         },
         
         // possible moves
         moves: {17: [[5,0], [5,1]],
         18: [[5,0], [5,1], [5,2]]}
         };
         */
    }
    
    function _initState(onDoneCallback, viewGameUrl, moveUrl) {

        _VIEWURL = viewGameUrl;
        _MOVEURL = moveUrl;
        var state = null;
        $.ajax({
            url: _VIEWURL,
            dataType: "json",
            method: "GET"
        })
        .done(function(data) {
            console.log("Received response: " + data);
            _updateState(data);
            onDoneCallback();
        });
    };

    function _fetchBoard() {
        return _board;
    };

    function _fetchSpace(coord) {

        var row = coord[0];
        var col = coord[1];

        return _board[row][col];
    };

    function _isValidMove(pId, row, col) {

        return _validMoves[row] != null
            && _validMoves[row][col] != null
            && _validMoves[row][col].indexOf(pId) != -1;
    }

    function _isMoveablePiece(pId) {
        return _moveablePieces[pId] == true;
    }

    function _registerBoardListener(boardCallback) {

        var that = this;
        nerve.on({
            channel: 'board-update',
            callback: boardCallback,
            scope: {that}
        });
    };

    function _fetchPlayerTurn() {
        return _playerTurn;
    };

    return {
        registerBoardListener: _registerBoardListener,
        initBoard: _initState,
        fetchBoard: _fetchBoard,
        fetchSpace: _fetchSpace,
        isValidMove: _isValidMove,
        isMoveablePiece: _isMoveablePiece,
        calculateColor: _calculateColor,
        move: _move,
        fetchPlayerTurn: _fetchPlayerTurn
    };
}();

