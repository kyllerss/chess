var ChessController = function() {

    var _markState = {};

    function _move(pId, newCoord) {

        var row = newCoord[0];
        var col = newCoord[1];
        var validMove = ChessModel.isValidMove(pId, row, col);

        if (validMove) {
            ChessModel.move(pId, newCoord);
        }

        _clearMark();
    };

    function _mark(pId) {

        _markState = {pId: pId};
        nerve.send({
            channel: 'mark',
            context: _markState
        });
    };

    function _clearMark() {

        _markState = {};
        nerve.send({
            channel: 'mark-clear',
            context: _markState
        });
    };

    function _registerMarkListeners(markCallback, clearMarkCallback) {

        var that = this;
        nerve.on({
            channel: 'mark',
            callback: markCallback,
            scope: {that}
        });

        nerve.on({
            channel: 'mark-clear',
            callback: clearMarkCallback,
            scope: {that}
        });
    };

    return {
        move: _move,
        mark: _mark,
        clearMark: _clearMark,
        registerMarkListeners: _registerMarkListeners
    };

}();
