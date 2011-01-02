package com.turbostool.client.geom.test {

import flexunit.framework.TestSuite;

public class GeomTestSuite extends TestSuite
{
    public function GeomTestSuite(param:Object = null)
    {
        super(param);
        this.addTestSuite(LocalFrame2dTest);
        this.addTestSuite(Piece2dTest);
        this.addTest(PieceIteratorTest.suite());
        this.addTestSuite(PolygonTest);
        this.addTest(RectangleTest.suite());
        this.addTestSuite(Vector2dSequenceTest);
    }

}
}