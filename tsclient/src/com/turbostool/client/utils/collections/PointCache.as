package com.turbostool.client.utils.collections {
import com.turbostool.client.utils.TSError;

import flash.geom.Point;

public class PointCache {
    private static const DEFAULT_ITEM_INCREMENT: Number = 10;

    private var myPoints: Array = new Array();
    private var mySize: Number = 0;

    public function PointCache() {
        enlargeMyArray();
    }

    private function enlargeMyArray(): void {
        for (var i: Number = 0; i < DEFAULT_ITEM_INCREMENT; i++) {
            myPoints.push(new Point());
        }
    }

    private function addItem(): int {
        if (mySize == myPoints.length) {
            enlargeMyArray();
        }
        mySize++;
        return mySize-1;
    }

    public function addAndGetNextFreeItem(): Point {
        return myPoints[addItem()] as Point;
    }

    public function getPoint(index: Number): Point {
        if (index < mySize) {
            return myPoints[index];
        }
        throw new TSError("cache index out of bounds");
    }

    public function size(): Number {
        return mySize;
    }

    public function clear(): void {
        mySize = 0;
    }

}
}