package com.turbostool.client.utils.collections {

import flash.geom.Point;

public class PointCollection {
    private var array: ArrayList = new ArrayList();

    private var minX: Number = Number.POSITIVE_INFINITY;
    private var maxX: Number = Number.NEGATIVE_INFINITY;

    private var minY: Number = Number.POSITIVE_INFINITY;
    private var maxY: Number = Number.NEGATIVE_INFINITY;

    public function addItem(p: Point): void {
        if (p.x > maxX) {
            maxX = p.x;
        }
        if (p.x < minX) {
            minX = p.x;
        }
        if (p.y > maxY) {
            maxY = p.y;
        }
        if (p.y < minY) {
            minY = p.y;
        }
        array.addItem(p);
    }

    public function clear(): void {
        array.clear();

        minX = Number.POSITIVE_INFINITY;
        maxX = Number.NEGATIVE_INFINITY;

        minY = Number.POSITIVE_INFINITY;
        maxY = Number.NEGATIVE_INFINITY;
    }

    public function iterator(): Iterator {
        return array.iterator();
    }

    public function length(): Number {
        return array.length();
    }

}
}