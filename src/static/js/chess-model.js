var ChessModel = (function () {

    var _this = {
        _rawState: {},
        _board: {},
        _pieceMapping: {p: "Pawn", r: "Rook", n: "Knight", b: "Bishop", q: "Queen", k: "King"},
        _colorMapping: {w: "White", b: "Black"},

        _calculateColor = function(color) {
            return inner._colorMapping[color];
        },

        _calculatePieceType = function(type) {
            return inner._pieceMapping[type];
        },

        _initState = function() {

            this._rawState = {

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
            this._board = [];
            for (var rowId = 0; rowId < this._rawState.board.length; rowId++) {

                var rawRow = this._rawState.board[rowId];

                var rowState = [];
                for (var colId = 0; colId < rawRow.length; colId++) {

                    var color = inner._calculateColor(rawRow[colId]);
                    var space = {color: color};

                    rowState.push(space);
                }

                this._board.push(rowState);
            }

            // place pieces
            for (var pId in this._rawState.pieces) {

                var rawPiece = this._rawState.pieces[pId];

                var rowCoord = rawPiece.xy[0];
                var colCoord = rawPiece.xy[1];

                var spaceState = this._board[rowCoord][colCoord];
                spaceState.pieceType = this._calculatePieceType(rawPiece.t);
                spaceState.pieceColor = this._calculateColor(rawPiece.c);
            }

            return this._board;
        },

        _fetchSpace: function(coord) {

            var row = coord[0];
            var col = coord[1];

            return this._board[row][col];
        },

        _registerListener: function(eventType, callback) {
            nerve.on(eventType, callback);
        }
    }();

    var inner = _this;
    return {
        register: inner._registerListener,
        initBoard: inner._initState,
        fetchSpace: inner._fetchSpace,
        inner: inner
    };
})();

