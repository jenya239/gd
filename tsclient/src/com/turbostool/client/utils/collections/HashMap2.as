package com.turbostool.client.utils.collections
{
public class HashMap2 extends HashMap
{
    public function HashMap2()
    {
    }

    protected override function createArrays(): void
    {
        myKeys = new ArrayList2();
        myValues = new ArrayList2();
    }

}
}