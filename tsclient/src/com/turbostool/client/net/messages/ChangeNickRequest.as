package com.turbostool.client.net.messages {
public class ChangeNickRequest extends ServerRequest {
	public static const CHANGE_NICK: String = "changeNick";

	private var _userId: int;
	private var _newNick: String;
	private var _forFree: Boolean;

	public function ChangeNickRequest(userId: int, newNick: String, forFree: Boolean){
		super(CHANGE_NICK);

		_userId = userId;
		_newNick = newNick;
		_forFree = forFree;
	}

	[Serializable(order=1)]
	public function get userId():int {
		return _userId;
	}

	[Serializable(order=2, escape=true)]
	public function get newNick():String {
		return _newNick;
	}

	[Serializable(order=3)]
	public function get forFree():Boolean {
		return _forFree;
	}
}
}