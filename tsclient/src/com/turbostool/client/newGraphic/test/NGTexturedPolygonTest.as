package com.turbostool.client.newGraphic.test
{

import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.geom.Polygon;
import com.turbostool.client.newGraphic.NGTexturedPolygon;
import com.turbostool.client.utils.Vector2d;

import flash.display.BitmapData;

import flexunit.framework.TestCase;

public class NGTexturedPolygonTest extends TestCase
{
    public function NGTexturedPolygonTest(methodName:String = null)
    {
        super(methodName);
    }

    public function testConstructor(): void {

        var testPolygon: Polygon = new Polygon();
        testPolygon.addVertex(new Vector2d(10, 10));
        testPolygon.addVertex(new Vector2d(20, 10));
        testPolygon.addVertex(new Vector2d(20, 20));
        testPolygon.addVertex(new Vector2d(10, 20));

        var testTexturedPolygon: NGTexturedPolygon = new NGTexturedPolygon(new LocalFrame2d(new Vector2d(100, 100)), 10, 1, testPolygon, new BitmapData(10, 10), 10, 10);
        assertTrue(testTexturedPolygon.myGeometricCenter.equals(new Vector2d(115, 115)));
    }
}
}