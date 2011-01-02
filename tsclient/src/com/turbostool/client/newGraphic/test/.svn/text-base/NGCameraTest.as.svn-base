package com.turbostool.client.newGraphic.test {
import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.newGraphic.NGCamera;
import com.turbostool.client.utils.TSTestCase;
import com.turbostool.client.utils.Vector2d;

import flash.geom.Matrix;

public class NGCameraTest extends TSTestCase {
    private var myCamera: NGCamera;

    override public function setUp():void {
        myCamera = new NGCamera(new LocalFrame2d(new Vector2d(1, 2), Math.PI / 6), 17, 550);
    }

    public function NGCameraTest(methodName:String = null) {
        super(methodName);
    }

//    public function testInverseTransform(): void {
//        var m: Matrix = myCamera.getTransform();
//        var im: Matrix = myCamera.getInverseTransform();
//        m.concat(im);
//        checkNumber(1, m.a, 'm.a');
//        checkNumber(0, m.b, 'm.b');
//        checkNumber(0, m.c, 'm.c');
//        checkNumber(1, m.d, 'm.d');
//        checkNumber(0, m.tx, 'm.tx');
//        checkNumber(0, m.ty, 'm.ty');
//    }
}
}