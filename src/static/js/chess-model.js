var ChessModel = function () {

    /*
     * TODO: Translate these to raw values w/ coordinates. Component will do property translation.
     */
    var _fetchGrid = function() {
        var spaces = [

            // Row 1
            [
            {color: "White",
             pieceType: "Rook",
             pieceColor: "Black"},
            {color: "Black",
             pieceType: "Knight",
             pieceColor: "Black"},
            {color: "White",
             pieceType: "Bishop",
             pieceColor: "Black"},
            {color: "Black",
             pieceType: "Queen",
             pieceColor: "Black"},
            {color: "White",
             pieceType: "King",
             pieceColor: "Black"},
            {color: "Black",
             pieceType: "Bishop",
             pieceColor: "Black"},
            {color: "White",
             pieceType: "Knight",
             pieceColor: "Black"},
            {color: "Black",
             pieceType: "Rook",
             pieceColor: "Black"}
            ],

            // Row 2
            [
            {color: "Black",
             pieceType: "Pawn",
             pieceColor: "Black"},
            {color: "White",
             pieceType: "Pawn",
             pieceColor: "Black"},
            {color: "Black",
             pieceType: "Pawn",
             pieceColor: "Black"},
            {color: "White",
             pieceType: "Pawn",
             pieceColor: "Black"},
            {color: "Black",
             pieceType: "Pawn",
             pieceColor: "Black"},
            {color: "White",
             pieceType: "Pawn",
             pieceColor: "Black"},
            {color: "Black",
             pieceType: "Pawn",
             pieceColor: "Black"},
            {color: "White",
             pieceType: "Pawn",
             pieceColor: "Black"}
            ],

            // Row 3
            [
            {color: "White"},
            {color: "Black"},
            {color: "White"},
            {color: "Black"},
            {color: "White"},
            {color: "Black"},
            {color: "White"},
            {color: "Black"}
            ],

            // Row 4
            [
            {color: "Black"},
            {color: "White"},
            {color: "Black"},
            {color: "White"},
            {color: "Black"},
            {color: "White"},
            {color: "Black"},
            {color: "White"}
            ],

            // Row 5
            [
            {color: "White"},
            {color: "Black"},
            {color: "White"},
            {color: "Black"},
            {color: "White"},
            {color: "Black"},
            {color: "White"},
            {color: "Black"}
            ],

            // Row 6
            [
            {color: "Black"},
            {color: "White"},
            {color: "Black"},
            {color: "White"},
            {color: "Black"},
            {color: "White"},
            {color: "Black"},
            {color: "White"}
            ],

            // Row 7
            [
            {color: "White",
             pieceType: "Pawn",
             pieceColor: "White"},
            {color: "Black",
             pieceType: "Pawn",
             pieceColor: "White"},
            {color: "White",
             pieceType: "Pawn",
             pieceColor: "White"},
            {color: "Black",
             pieceType: "Pawn",
             pieceColor: "White"},
            {color: "White",
             pieceType: "Pawn",
             pieceColor: "White"},
            {color: "Black",
             pieceType: "Pawn",
             pieceColor: "White"},
            {color: "White",
             pieceType: "Pawn",
             pieceColor: "White"},
            {color: "Black",
             pieceType: "Pawn",
             pieceColor: "White"}
            ],

            // Row 8
            [
            {color: "Black",
             pieceType: "Rook",
             pieceColor: "White"},
            {color: "White",
             pieceType: "Knight",
             pieceColor: "White"},
            {color: "Black",
             pieceType: "Bishop",
             pieceColor: "White"},
            {color: "White",
             pieceType: "King",
             pieceColor: "White"},
            {color: "Black",
             pieceType: "Queen",
             pieceColor: "White"},
            {color: "White",
             pieceType: "Bishop",
             pieceColor: "White"},
            {color: "Black",
             pieceType: "Knight",
             pieceColor: "White"},
            {color: "White",
             pieceType: "Rook",
             pieceColor: "White"}
            ]
        ];

        return spaces;
    };

    var _registerListener = function(eventType, callback) {
        nerve.on(eventType, callback);
    };

    return {
        fetchBoard: _fetchGrid,
        register: _registerListener
    };
}();

