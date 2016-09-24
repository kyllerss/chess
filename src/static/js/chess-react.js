var Board = React.createClass({

    _fetchGrid: function() {
        return ChessModel.fetchBoard();
    },

    render: function() {

        var grid = this._fetchGrid();

        var spaces = [];
        for (var i = 0; i < grid.length; i++) {

            var row = grid[i];
            for (var j = 0; j < row.length; j++) {

                var props = row[j];
                var key = "space-" + i + "-" + j;
                props['key'] = key;
                props['row'] = i;
                props['column'] = j;

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
        color: React.PropTypes.string.isRequired,
        row: React.PropTypes.number.isRequired,
        column: React.PropTypes.number.isRequired
    },

    render: function() {

        var pType = this.props.pieceType;
        var pColor = this.props.pieceColor;
        var row = this.props.row;
        var column = this.props.column;

        var pProps = null;
        if (pType != null && pColor != null) {
            pProps = {pieceType: pType, pieceColor: pColor};
        }

        var piece = React.createElement(Piece, pProps);

        var sColor = this.props.color === "White" ? "white" : "black";
        var className = "c-space " + sColor;

        var props = {className: className,
                     'data-row': row,
                     'data-column': column};
        return React.DOM.div(props, piece);
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
