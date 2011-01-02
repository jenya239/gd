package com.turbostool.client.newGraphic {

import flash.geom.Matrix;
import flash.geom.Point;

public class MatrixStack {

    private static const DEFAULT_ITEM_INCREMENT: Number = 10;
    private var _stack: Array = new Array();
    private var _currentIndex: int = 0;

    public function MatrixStack() {
        enlargeMyArray();
    }

    private function enlargeMyArray(): void {
        for (var i: Number = 0; i < DEFAULT_ITEM_INCREMENT; i++) {
            _stack.push(new Matrix());
        }
    }

    public function pushMatrix(m: Matrix): void {
        if (_currentIndex >= _stack.length-1) {
            enlargeMyArray();
        }
        _currentIndex++;
        var prevProduct: Matrix = _stack[_currentIndex-1] as Matrix;
        var newProduct: Matrix = _stack[_currentIndex] as Matrix;
        NGGraphics.copyMatrix(m, newProduct);
        newProduct.concat(prevProduct);
    }

    public function popMatrix(): void {
        _currentIndex--;
    }

    public function getProduct(): Matrix {

        return _stack[_currentIndex] as Matrix;

    }
}
}