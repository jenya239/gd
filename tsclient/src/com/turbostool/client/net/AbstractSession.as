package com.turbostool.client.net {
public class AbstractSession {
    //		private var myVars:Object;
    //
    //		public function AbstractSession() {
    //			myVars = new Object();
    //		}
    //
    //		public function varExists(varName:String):Boolean {
    //			return myVars[varName] != undefined;
    //		}
    //
    //		private function testAndGetFloat(name:String): Number {
    //			if (varExists(name)) {
    //				var res: Number = parseFloat(getVarValue(name));
    //				return res;
    //			}
    //			return 0;
    //		}
    //
    //		private function testAndGetUint(name:String): uint{
    //			if (varExists(name)) {
    //				return uint(getVarValue(name));
    //			}
    //			return 0;
    //		}
    //
    //		private function testAndGetString(name:String): String {
    //			if (varExists(name)) {
    //				return getVarValue(name);
    //			}
    //			return 'UNDEFINED';
    //		}
    //
    //		public function setVar(name:String, value:String):void {
    //			myVars[name] = value;
    //			myVars.setPropertyIsEnumerable(name, true);
    //		}
    //
    //		public function getVarValue(name:String):String {
    //			Assert.assertTrue(varExists(name));
    //			var res: String = myVars[name];
    //			return res;
    //		}
    //
    //		public function unsetVar(name:String):void {
    //		//	Assert.assertTrue(varExists(name));
    //			forceUnsetVar(name);
    //		}
    //
    //		public function forceUnsetVar(name:String):void {
    //			delete myVars[name];
    //		}
    //
    ////		public function getVars():Object {
    ////			var arr:Object = new Object();
    ////			for (var p:String in myVars) {
    ////				arr[p] = myVars[p];
    ////				arr.setPropertyIsEnumerable(p, true);
    ////			}
    ////			return arr;
    ////		}
    //
    //		public function get myColor(): uint {
    //			return testAndGetUint(SetVarEvent.CHAT_COLOR_NAME);
    //		}
    //
    //		public function get myBestTime(): Number {
    //			return testAndGetFloat(SetVarEvent.BEST_TIME_NAME);
    //		}
    //
    //		public function get myLastTime(): Number {
    //			var res: Number = testAndGetFloat(SetVarEvent.LAST_TIME_NAME);
    //			return res;
    //		}
    //
    //		public function get myStartTime(): Number {
    //			return testAndGetFloat(SetVarEvent.START_TIME_NAME);
    //		}
    //
    //		public function set myStartTime(time: Number): void {
    //			setVar(SetVarEvent.START_TIME_NAME,time.toString());
    //		}
    //
    //		public function get myName(): String {
    //			return testAndGetString(SetVarEvent.CHAT_NAME_NAME);
    //		}
    //
    //		public function get myRouteName(): String {
    //			return testAndGetString(SetVarEvent.ROUTE_NAME_NAME);
    //		}
}
}