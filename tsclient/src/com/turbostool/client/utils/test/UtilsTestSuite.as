package com.turbostool.client.utils.test {

import flexunit.framework.TestSuite;

public class UtilsTestSuite extends TestSuite {
    public function UtilsTestSuite() {
        super();
        addTestSuite(UtilsTest);
        addTestSuite(Vector2dTest);
        addTestSuite(Vector3dTest);
        addTestSuite(Matrix2dTest);
    }
}
}