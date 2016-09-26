var ChessController = function() {

    var _markState = {};

    function _move(pieceId, newCoord) {

        _clearMark();
    };

    function _mark(pId) {

        _markState = {pId: pId};
        nerve.send('mark', _markState);
    };

    function _clearMark() {
        _markState = {};
        nerve.send('mark-clear', _markState);
    };

    function _registerMarkListeners(markCallback, clearMarkCallback) {

        nerve.on('mark', markCallback);
        nerve.on('mark-clear', clearMarkCallback);
    };

    return {
        move: _move,
        mark: _mark,
        clearMark: _clearMark,
        registerMarkListeners: _registerMarkListeners
    };

}();
