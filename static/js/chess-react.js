var Game = React.createClass({

    /*
    getInitialState: function() {

        var grid = ChessModel.fetchBoard();
        return {board: grid};
    },
    */
    render: function() {

        var playerMarker = React.createElement(PlayerMarker);
        var separator = React.DOM.div({className: "clear-float"});
        var board = React.createElement(Board);

        return React.DOM.div({className: "c-game"}, [playerMarker, separator, board]);
    }
});

var PlayerMarker = React.createClass({

    getInitialState: function() {

        ChessModel.registerBoardListener(this._gameUpdate);

        var playerTurn = ChessModel.fetchPlayerTurn();
        return {playerTurn: playerTurn};
    },

    _gameUpdate: function(state) {

        var playerTurn = ChessModel.fetchPlayerTurn();
        this.setState({playerTurn: playerTurn});
    },

    render: function() {

        var player = ChessModel.calculateColor(this.state.playerTurn.color);
        console.log("Updated player turn: " + player);

        return React.DOM.div({className: "c-playerMarker"}, "Player " + player);
    }
});

var Board = React.createClass({

    getInitialState: function() {

        var grid = ChessModel.fetchBoard();
        return {board: grid};
    },

    render: function() {

        var grid = this.state.board;

        var rowElements = [];
        for (var i = 0; i < grid.length; i++) {

            var colElements = [];
            var cols = grid[i];
            for (var j = 0; j < cols.length; j++) {

                var props = cols[j];
                var key = "space-" + i + "-" + j;
                props['key'] = key;
                props['row'] = i;
                props['column'] = j;

                colElements.push(React.createElement(Space, props));
            }

            rowElements.push(React.DOM.div({className: "c-row"}, colElements));
        }

        return React.DOM.div({className: "c-board"}, rowElements);
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
        //console.log("_onDrop: [" + this.props.row + "," + this.props.column + "] + " + pId + " = " + validMove);

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

        //console.log("_mark: [" + this.props.row + "," + this.props.column + "] + " + pId + " = " + validMove);
    },

    _clearMark: function(state) {
        this.setState({highlightedMoveTarget: null});
        //console.log("_clearMark: " + pId + " -> [" + this.props.row + "," + this.props.column + "]");
    },

    _boardUpdate: function(state) {

        var space = ChessModel.fetchSpace([this.props.row, this.props.column]);
        this.setState({
            pieceType: space.pieceType,
            pieceColor: space.pieceColor,
            pieceId: space.pieceId
        });
    },

    // acts as constructor (http://stackoverflow.com/questions/33526493/react-createclass-vs-extends-component)
    getInitialState: function() {

        //console.log("getInitialState  -> [" + this.props.row + "," + this.props.column + "]");

        ChessController.registerMarkListeners(this._mark, this._clearMark);
        ChessModel.registerBoardListener(this._boardUpdate);

        var pType = this.props.pieceType;
        var pColor = this.props.pieceColor;
        var pId = this.props.pieceId;

        return {
            highlightedMoveTarget: null,
            pieceType: pType,
            pieceColor: pColor,
            pieceId: pId
        };
    },

    render: function() {

        var pType = this.state.pieceType;
        var pColor = this.state.pieceColor;
        var pId = this.state.pieceId;
        var row = this.props.row;
        var column = this.props.column;

        var piece = null;
        if (pId != null) {
            var pProps = {pieceType: pType,
                          pieceColor: pColor,
                          pieceId: pId};
            piece = React.createElement(Piece, pProps);
        }

        var sColorClass;
        var calcColor = this.props.color;
        if (calcColor === "White") {
            sColorClass = "white";
        } else if (calcColor === "Black") {
            sColorClass = "black";
        } else if (calcColor === "None") {
            sColorClass = "none";
        } else {
            sColorClass = "";
        }
        var highlighted = this.state.highlightedMoveTarget ? "highlighted-move-target" : "";
        var nonHighlighted = this.state.highlightedMoveTarget == false ? "non-highlighted-move-target" : "";
        var className = "c-space " + sColorClass + " " + highlighted + " " + nonHighlighted;

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
        //console.log("onDragStart: " + this.props.pieceId);
        event.dataTransfer.setData("pId", this.props.pieceId);

        ChessController.mark(this.props.pieceId);
    },

    render: function() {

        var pType = this.props.pieceType;
        var pColor = this.props.pieceColor;
        var pId = this.props.pieceId;

        var piece;
        if (pColor == "White") {
            
            if (pType === "Pawn") {
                piece = '&#9817;';
            } else if (pType === "Rook") {
                piece = '&#9814;';
            } else if (pType === "Knight") {
                piece = '&#9816;';
            } else if (pType === "Bishop") {
                piece = '&#9815;';
            } else if (pType === "Queen") {
                piece = '&#9813;';
            } else if (pType === "King") {
                piece = '&#9812;';
            } else {
                piece = "";
            }

        } else {
            
            if (pType === "Pawn") {
                piece = '&#9823;';
            } else if (pType === "Rook") {
                piece = '&#9820;';
            } else if (pType === "Knight") {
                piece = '&#9822;';
            } else if (pType === "Bishop") {
                piece = '&#9821;';
            } else if (pType === "Queen") {
                piece = '&#9819;';
            } else if (pType === "King") {
                piece = '&#9818;';
            } else {
                piece = "";
            }
        }

        var colorClass;
        if (pColor === "Black") {
            colorClass = "c-black";
        } else if (pColor === "Green") {
            colorClass = "c-green";
        } else if (pColor === "Blue") {
            colorClass = "c-blue";
        } else if (pColor === "Purple") {
            colorClass = "c-purple";
        } else if (pColor === "Orange") {
            colorClass = "c-orange";
        } else if (pColor === "Yellow") {
            colorClass = "c-yellow";
        } else if (pColor === "Red") {
            colorClass = "c-red";
        } else {
            colorClass = "";
        }

        var draggable = ChessModel.isMoveablePiece(pId);
        var properties = {className: "c-piece" + " " + colorClass,
                          dangerouslySetInnerHTML: {__html: piece},
                          draggable: draggable,
                          onDragStart: this._startDrag};

        if (pId != null) {
            properties['data-pid'] = pId;
            //console.log("React piece id: " + pId);
        }

        return React.DOM.div(properties, null);
    }
});
