package com.turbostool.client.utils {
public class Assert {
    public static function assertEquals(... rest):void {
        if (rest.length == 3)
            failNotEquals(rest[0], rest[1], rest[2]);
        else
            failNotEquals("", rest[0], rest[1]);
    }

    public static function assertNotEquals(... rest):void {
        if (rest.length == 3)
            failEquals(rest[0], rest[1], rest[2]);
        else
            failEquals("", rest[0], rest[1]);
    }

    private static function failNotEquals(message:String, expected:Object, actual:Object):void {
        if (expected != actual)
            failWithUserMessage(message, "expected:<" + expected + "> but was:<" + actual + ">");
    }

    private static function failEquals(message:String, expected:Object, actual:Object):void {
        if (expected == actual)
            failWithUserMessage(message, "unexpected value:<" + expected + ">");
    }

    public static function assertStrictlyEquals(... rest):void {
        if (rest.length == 3)
            failNotStrictlyEquals(rest[0], rest[1], rest[2]);
        else
            failNotStrictlyEquals("", rest[0], rest[1]);
    }

    private static function failNotStrictlyEquals(message:String, expected:Object, actual:Object):void {
        if (expected !== actual)
            failWithUserMessage(message, "expected:<" + expected + "> but was:<" + actual + ">");
    }

    public static function assertTrue(... rest):void {
        if (rest.length == 2)
            failNotTrue(rest[0], rest[1]);
        else
            failNotTrue("", rest[0]);
    }

    private static function failNotTrue(message:String, condition:Boolean):void {
        if (!condition)
            failWithUserMessage(message, "expected true but was false");
    }

    public static function assertFalse(... rest):void {
        if (rest.length == 2)
            failTrue(rest[0], rest[1]);
        else
            failTrue("", rest[0]);
    }

    private static function failTrue(message:String, condition:Boolean):void {
        if (condition)
            failWithUserMessage(message, "expected false but was true");
    }

    public static function assertNull(... rest):void {
        if (rest.length == 2)
            failNotNull(rest[0], rest[1]);
        else
            failNotNull("", rest[0]);
    }

    private static function failNull(message:String, object:Object):void {
        if (object == null)
            failWithUserMessage(message, "object was null: " + object);
    }

    public static function assertNotNull(... rest):void {
        if (rest.length == 2)
            failNull(rest[0], rest[1]);
        else
            failNull("", rest[0]);
    }

    private static function failNotNull(message:String, object:Object):void {
        if (object != null)
            failWithUserMessage(message, "object was not null: " + object);
    }

    public static function fail(failMessage:String = null):void {
        throw new AssertionFailedError(failMessage);
    }

    private static function failWithUserMessage(userMessage:String, failMessage:String):void {
        if (userMessage.length > 0)
            userMessage = userMessage + " - ";

        throw new AssertionFailedError(userMessage + failMessage);
    }

    public static function assertSingleton(instance: Object): void
    {
        if (instance != null)
        {
            throw new AssertionFailedError(Utils.stringFormat("Попытка вызвать конструктор синглетона {0}", Utils.getClassName(instance)));
        }
    }
}
}