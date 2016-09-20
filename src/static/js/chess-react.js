var Board = React.createClass({

    render: function() {

        var spaces = [

            // Row 1
            React.createElement(Space, {color: "White",
                                        pieceType: "Rook",
                                        pieceColor: "Black"}),
            React.createElement(Space, {color: "Black",
                                        pieceType: "Knight",
                                        pieceColor: "Black"}),
            React.createElement(Space, {color: "White",
                                        pieceType: "Bishop",
                                        pieceColor: "Black"}),
            React.createElement(Space, {color: "Black",
                                        pieceType: "Queen",
                                        pieceColor: "Black"}),
            React.createElement(Space, {color: "White",
                                        pieceType: "King",
                                        pieceColor: "Black"}),
            React.createElement(Space, {color: "Black",
                                        pieceType: "Bishop",
                                        pieceColor: "Black"}),
            React.createElement(Space, {color: "White",
                                        pieceType: "Knight",
                                        pieceColor: "Black"}),
            React.createElement(Space, {color: "Black",
                                        pieceType: "Rook",
                                        pieceColor: "Black"}),

            // Row 2
            React.createElement(Space, {color: "Black",
                                        pieceType: "Pawn",
                                        pieceColor: "Black"}),
            React.createElement(Space, {color: "White",
                                        pieceType: "Pawn",
                                        pieceColor: "Black"}),
            React.createElement(Space, {color: "Black",
                                        pieceType: "Pawn",
                                        pieceColor: "Black"}),
            React.createElement(Space, {color: "White",
                                        pieceType: "Pawn",
                                        pieceColor: "Black"}),
            React.createElement(Space, {color: "Black",
                                        pieceType: "Pawn",
                                        pieceColor: "Black"}),
            React.createElement(Space, {color: "White",
                                        pieceType: "Pawn",
                                        pieceColor: "Black"}),
            React.createElement(Space, {color: "Black",
                                        pieceType: "Pawn",
                                        pieceColor: "Black"}),
            React.createElement(Space, {color: "White",
                                        pieceType: "Pawn",
                                        pieceColor: "Black"}),

            // Row 3
            React.createElement(Space, {color: "White"}),
            React.createElement(Space, {color: "Black"}),
            React.createElement(Space, {color: "White"}),
            React.createElement(Space, {color: "Black"}),
            React.createElement(Space, {color: "White"}),
            React.createElement(Space, {color: "Black"}),
            React.createElement(Space, {color: "White"}),
            React.createElement(Space, {color: "Black"}),

            // Row 4
            React.createElement(Space, {color: "Black"}),
            React.createElement(Space, {color: "White"}),
            React.createElement(Space, {color: "Black"}),
            React.createElement(Space, {color: "White"}),
            React.createElement(Space, {color: "Black"}),
            React.createElement(Space, {color: "White"}),
            React.createElement(Space, {color: "Black"}),
            React.createElement(Space, {color: "White"}),

            // Row 5
            React.createElement(Space, {color: "White"}),
            React.createElement(Space, {color: "Black"}),
            React.createElement(Space, {color: "White"}),
            React.createElement(Space, {color: "Black"}),
            React.createElement(Space, {color: "White"}),
            React.createElement(Space, {color: "Black"}),
            React.createElement(Space, {color: "White"}),
            React.createElement(Space, {color: "Black"}),

            // Row 6
            React.createElement(Space, {color: "Black"}),
            React.createElement(Space, {color: "White"}),
            React.createElement(Space, {color: "Black"}),
            React.createElement(Space, {color: "White"}),
            React.createElement(Space, {color: "Black"}),
            React.createElement(Space, {color: "White"}),
            React.createElement(Space, {color: "Black"}),
            React.createElement(Space, {color: "White"}),

            // Row 7
            React.createElement(Space, {color: "White",
                                        pieceType: "Pawn",
                                        pieceColor: "White"}),
            React.createElement(Space, {color: "Black",
                                        pieceType: "Pawn",
                                        pieceColor: "White"}),
            React.createElement(Space, {color: "White",
                                        pieceType: "Pawn",
                                        pieceColor: "White"}),
            React.createElement(Space, {color: "Black",
                                        pieceType: "Pawn",
                                        pieceColor: "White"}),
            React.createElement(Space, {color: "White",
                                        pieceType: "Pawn",
                                        pieceColor: "White"}),
            React.createElement(Space, {color: "Black",
                                        pieceType: "Pawn",
                                        pieceColor: "White"}),
            React.createElement(Space, {color: "White",
                                        pieceType: "Pawn",
                                        pieceColor: "White"}),
            React.createElement(Space, {color: "Black",
                                        pieceType: "Pawn",
                                        pieceColor: "White"}),

            // Row 8
            React.createElement(Space, {color: "Black",
                                        pieceType: "Rook",
                                        pieceColor: "White"}),
            React.createElement(Space, {color: "White",
                                        pieceType: "Knight",
                                        pieceColor: "White"}),
            React.createElement(Space, {color: "Black",
                                        pieceType: "Bishop",
                                        pieceColor: "White"}),
            React.createElement(Space, {color: "White",
                                        pieceType: "King",
                                        pieceColor: "White"}),
            React.createElement(Space, {color: "Black",
                                        pieceType: "Queen",
                                        pieceColor: "White"}),
            React.createElement(Space, {color: "White",
                                        pieceType: "Bishop",
                                        pieceColor: "White"}),
            React.createElement(Space, {color: "Black",
                                        pieceType: "Knight",
                                        pieceColor: "White"}),
            React.createElement(Space, {color: "White",
                                        pieceType: "Rook",
                                        pieceColor: "White"}),
        ];

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

        return React.createElement("div", {className: "c-piece", dangerouslySetInnerHTML: {__html: piece}});
    }
});
