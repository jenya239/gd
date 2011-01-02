package com.turbostool.client.newGraphic.test {

import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.newGraphic.NGCamera;
import com.turbostool.client.newGraphic.NGGraphicEngine;
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.Vector2d;

import flash.geom.Matrix;

import flexunit.framework.TestCase;

import mx.containers.Canvas;

public class NGGraphicEngineTest extends TestCase
{
    public function NGGraphicEngineTest(methodName:String = null) {
        super(methodName);
    }

    private function checkNumber(expected:Number, value:Number, name:String):void {
        assertTrue(name + ' = ' + value + ' а не ' + expected, Utils.equal(expected, value));
    }

    public function testInverseTransform(): void {
        var bitmap: Canvas = new Canvas();
        bitmap.width = 200;
        bitmap.height = 100;
        var camera: NGCamera = new NGCamera(new LocalFrame2d(new Vector2d(1, 2), 34), 20, 500);
        var ngge: NGGraphicEngine = new NGGraphicEngine(camera);

        var m: Matrix = ngge.worldToScreen();
        m.concat(ngge.screenToWorld());

        checkNumber(1, m.a, 'm.a');
        checkNumber(0, m.b, 'm.b');
        checkNumber(0, m.c, 'm.c');
        checkNumber(1, m.d, 'm.d');
        checkNumber(0, m.tx, 'm.tx');
        checkNumber(0, m.ty, 'm.ty');
    }
}
}