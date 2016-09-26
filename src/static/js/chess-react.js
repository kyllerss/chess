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
        pieceId: React.PropTypes.string,
        color: React.PropTypes.string.isRequired,
        row: React.PropTypes.number.isRequired,
        column: React.PropTypes.number.isRequired
    },

    _isValidMoveTarget: function(event) {

        var pId = event.dataTransfer.getData("pId");
        if (pId == '') {
            return false;
        }

        var row = this.props.row;
        var column = this.props.column;
        var validMove = ChessModel.isValidMove(pId, row, column);

        return validMove;
    },

    _onDragOver: function(event) {

        if (event.preventDefault) {
            event.preventDefault();
        }

        return false;
    },

    _onDrop: function(event) {

        event.preventDefault();

        var validMove = this._isValidMoveTarget(event);
        var pId = event.dataTransfer.getData("pId");
        console.log("onDrop: " + validMove + " - " + pId + " -> [" + this.props.row + "," + this.props.column + "]");

        ChessController.move(pId, [this.props.row, this.props.column]);
    },

    _mark: function(state) {

        var pId = state.pId;
        var validMove = ChessModel.isValidMove(pId, this.props.row, this.props.column);
        if (validMove) {
            this.setState({highlightedMoveTarget: true});
        } else {
            this.setState({highlightedMoveTarget: false});
        }

        console.log("_mark: " + validMove + " - " + pId + " -> [" + this.props.row + "," + this.props.column + "]");
    },

    _clearMark: function(state) {
        this.setState({highlightedMoveTarget: false});
        console.log("_clearMark: " + pId + " -> [" + this.props.row + "," + this.props.column + "]");
    },

    // acts as constructor (http://stackoverflow.com/questions/33526493/react-createclass-vs-extends-component)
    getInitialState: function() {

        console.log("getInitialState  -> [" + this.props.row + "," + this.props.column + "]");
        ChessController.registerMarkListeners(this._mark, this._clearMark);
        ChessModel.registerBoardListener(this._boardUpdate);

        return {highlightedMoveTarget: false};
    },

    render: function() {

        var pType = this.props.pieceType;
        var pColor = this.props.pieceColor;
        var pId = this.props.pieceId;
        var row = this.props.row;
        var column = this.props.column;

        var piece = null;
        if (pId != null) {
            var pProps = {pieceType: pType,
                          pieceColor: pColor,
                          pieceId: pId};
            piece = React.createElement(Piece, pProps);
        }

        var sColor = this.props.color === "White" ? "white" : "black";
        var highlighted = this.state.highlightedMoveTarget ? "highlighted-move-target" : "";
        var className = "c-space " + sColor + " " + highlighted;

        var props = {className: className,
                     'data-row': row,
                     'data-column': column,
                     onDragOver: this._onDragOver,
                     onDrop: this._onDrop};
        return React.DOM.div(props, piece);
    }
});

var Piece = React.createClass({

    _startDrag: function(event) {
        console.log("onDragStart: " + this.props.pieceId);
        event.dataTransfer.setData("pId", this.props.pieceId);

        ChessController.mark(this.props.pieceId);
    },

    render: function() {

        var pType = this.props.pieceType;
        var pColor = this.props.pieceColor;
        var pId = this.props.pieceId;

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

        if (pId != null) {
            properties['data-pid'] = pId;
            console.log("Piece id: " + pId);
        }

        return React.DOM.div(properties, null);
    }
});
