var Board = React.createClass({

    _fetchGrid: function() {

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
    },

    render: function() {


        var grid = this._fetchGrid();

        var spaces = [];
        for (var i = 0; i < grid.length; i++) {

            var row = grid[i];
            for (var j = 0; j < row.length; j++) {

                var props = row[j];
                var key = "space-" + i + "-" + j;
                props.key = key;

                spaces.push(React.createElement(Space, props));
            }
        }

        return React.DOM.div({className: "c-board"}, spaces);
    } 
});

var Space = React.createClass({

    propTypes: {
        pieceType: React.PropTypes.string, // convert to an object param
        pieceColor: React.PropTypes.string,
        color: React.PropTypes.string.isRequired
    },

    render: function() {

        var pType = this.props.pieceType;
        var pColor = this.props.pieceColor;

        var pProps = null;
        if (pType != null && pColor != null) {
            pProps = {pieceType: pType, pieceColor: pColor};
        }

        var piece = React.createElement(Piece, pProps);

        var sColor = this.props.color === "White" ? "white" : "black";
        var className = "c-space " + sColor;

        return React.DOM.div({className: className}, piece);
    }
});

var Piece = React.createClass({

    _startDrag: function(event) {
        console.log(event);
    },

    render: function() {

        var pType = this.props.pieceType;
        var pColor = this.props.pieceColor;

        var piece;
        if (pType === "Pawn" && pColor === "White") {
            piece = '&#9817;';
        } else if (pType === "Pawn" && pColor === "Black") {
            piece = '&#9823;';
        } else if (pType === "Rook" && pColor === "White") {
            piece = '&#9814;';
        } else if (pType === "Rook" && pColor === "Black") {
            piece = '&#9820;';
        } else if (pType === "Knight" && pColor === "White") {
            piece = '&#9816;';
        } else if (pType === "Knight" && pColor === "Black") {
            piece = '&#9822;';
        } else if (pType === "Bishop" && pColor === "White") {
            piece = '&#9815;';
        } else if (pType === "Bishop" && pColor === "Black") {
            piece = '&#9821;';
        } else if (pType === "Queen" && pColor === "White") {
            piece = '&#9813;';
        } else if (pType === "Queen" && pColor === "Black") {
            piece = '&#9819;';
        } else if (pType === "King" && pColor === "White") {
            piece = '&#9812;';
        } else if (pType === "King" && pColor === "Black") {
            piece = '&#9818;';
        } else {
            piece = "";
        }

        var properties = {className: "c-piece",
                          dangerouslySetInnerHTML: {__html: piece},
                          draggable: "true",
                          onDragStart: this._startDrag};

        return React.createElement("div", properties);
    }
});
