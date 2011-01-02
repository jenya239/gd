package com.turbostool.client.net.test
{

import com.turbostool.client.net.SessionSocket;

import flash.utils.ByteArray;

import flexunit.framework.TestCase;
import flexunit.framework.TestSuite;

public class SessionSocketTest extends TestCase
{
    private var mySocket:SessionSocket;

    override public function setUp():void
    {
        mySocket = new SessionSocket();
    }

    public function SessionSocketTest(methodName:String)
    {
        super(methodName);
    }

    public static function suite():TestSuite
    {
        var ts:TestSuite = new TestSuite();
        ts.addTest(new SessionSocketTest("testSocketData"));
        //ts.addTest( new SessionSocketTest( "testRemoteSessions" ) );
        return ts;
    }

    public function testSocketData():void
    {
        var arr:ByteArray = new ByteArray();
        arr.writeShort(13);
        arr.writeByte(-128);
        arr.writeUTFBytes('<connected/>');
        arr.position = 0;
        assertEquals(13, arr.readShort());
        assertEquals(-128, arr.readByte());
        var bytes:ByteArray = new ByteArray();
        arr.readBytes(bytes, 0, 12);
        assertEquals('<connected/>', bytes.toString());
        assertEquals(0, arr.bytesAvailable);
    }

    //		public function testRemoteSessions():void {
    //			var session:RemoteSession;
    //			function addSess(event:RemoteSessionEvent):void {
    //				session = event.getSession();
    //				assertTrue(mySocket.sessionExists(98));
    //				assertEquals(session, mySocket.getSession(98));
    //				assertFalse(session.varExists('testN'));
    //				mySocket.dispatchEvent(new VarDataEvent(VarDataEvent.VAR_DATA, 98, 'testN', 'testV'));
    //			}
    //			function setVar(event22:Event):void {
    //				var event2:RemoteSessionVarEvent = event22 as RemoteSessionVarEvent;
    //				assertEquals(session, event2.getSession());
    //				assertTrue(session.varExists(event2.getName()));
    //				assertEquals(event2.getValue(), session.getVarValue(event2.getName()));
    //				mySocket.dispatchEvent(new VarDataEvent(VarDataEvent.VAR_DATA, 98, 'testN',
    //					VarDataEvent.UNDEFINED_VALUE
    //				));
    //			}
    //			function unsetVar(event3:RemoteSessionVarEvent):void {
    //				assertEquals(session, event3.getSession());
    //				assertFalse(session.varExists(event3.getName()));
    //				assertFalse(session.varExists('testN'));
    //				mySocket.dispatchEvent(new SessionDataEvent(SessionDataEvent.REMOVE_SESSION, 98));
    //			}
    //			function remSess(event4:RemoteSessionEvent):void {
    //				assertFalse(mySocket.sessionExists(98));
    //			}
    //			mySocket.addEventListener(RemoteSessionEvent.REMOVE_SESSION, addAsync(remSess, 30));
    //			mySocket.addEventListener(RemoteSessionVarEvent.UNSET_VAR, unsetVar);
    //			mySocket.addEventListener(RemoteSessionVarEvent.SET_VAR, setVar);
    //			mySocket.addEventListener(RemoteSessionEvent.ADD_SESSION, addSess);
    //			mySocket.dispatchEvent(new SessionDataEvent(SessionDataEvent.ADD_SESSION, 98));
    //		}
}
}