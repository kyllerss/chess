var ChessModel = (function () {

    var _rawState = {};
    var _board = {};
    var _validMoves = {};
    var _pieceMapping = {p: "Pawn", r: "Rook", n: "Knight", b: "Bishop", q: "Queen", k: "King"};
    var _colorMapping = {w: "White", b: "Black"};

    function _addValidMove(pId, move) {

        /*
             {1: {1: [1,2,3],
                 {2: [4,3]},
              2: {1: [3,3],
                  3: [3]}}

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

    function _initState() {

        _rawState = {

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
        _validMoves = {};
        for (pId in _rawState.moves) {

            var moves = _rawState.moves[pId];
            for (var i = 0; i < moves.length; i++) {

                var move = moves[i];
                _addValidMove(pId, move);
            }
        }
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

    function _registerBoardListener(callback) {
        nerve.on('board-update', callback);
    };

    return {
        registerBoardListener: _registerBoardListener,
        initBoard: _initState,
        fetchBoard: _fetchBoard,
        fetchSpace: _fetchSpace,
        isValidMove: _isValidMove
    };
})();

