package {
import flash.display.Sprite;
import flash.events.Event;
import flash.events.HTTPStatusEvent;
import flash.events.MouseEvent;
import flash.events.ProgressEvent;
import flash.media.Sound;
import flash.media.SoundLoaderContext;
import flash.media.SoundTransform;
import flash.net.NetConnection;
import flash.net.NetStream;
import flash.net.Socket;
import flash.net.URLRequest;
import flash.net.URLRequestHeader;
import flash.net.URLRequestMethod;
import flash.net.URLStream;
import flash.system.Security;
import flash.text.TextField;

import flash.utils.ByteArray;

import fly.sound.*;

import mx.controls.Alert;

public class RadioTest extends Sprite {
	public function RadioTest(){
		var t: TextField = new TextField();
		t.width = 600;
		t.height = 600;
		addChild(t);
		Security.loadPolicyFile('xmlsocket://188.93.17.232:843');
		var sp: MP3StreamPlayer = new MP3StreamPlayer();
		sp.playStream('http://188.93.17.232:2007/1');

//		Alert.show('sd=========');
//		var sock: Socket = new Socket();
//		sock.addEventListener(Event.CONNECT, function(e: Event):void{
//			//t.appendText('connect');
//			Alert.show('sdsdsd');
//		});
//		sock.addEventListener(ProgressEvent.SOCKET_DATA, function(e: ProgressEvent):void{
//			//t.appendText('connect');
//			//Alert.show('sdsdsd');
//			t.text = 'pp' + e.bytesLoaded;
//		});
//		//sock.connect('188.93.17.232', 2007);
//		sock.connect('127.0.0.1', 8888);

		 

	/*	var r: URLRequest = new URLRequest('http://188.93.17.232:2007/1');
		r.method = URLRequestMethod.POST;
		r.requestHeaders = new Array(new URLRequestHeader('Icy-MetaData', '1'));
		var us: URLStream = new URLStream();
		us.addEventListener(HTTPStatusEvent.HTTP_STATUS, function(e: HTTPStatusEvent): void{
			t.text = 'ddddd' + e.toString();
		});
		var data: ByteArray = new ByteArray();
		const MP3_LEN: int = 16000;
		const META_LEN: int = 16 * 55;
		const ALL_LEN: int = MP3_LEN + META_LEN + 1;
		//var mp3_waiting: Boolean = true;
		us.addEventListener(ProgressEvent.PROGRESS, function(e: ProgressEvent): void{
			//t.text += us.readUTF();
			us.readBytes(data, data.length);
			if( data.length > ALL_LEN ){
				data.position = MP3_LEN;
				var meta: String = data.readUTF();
				t.appendText('==' + meta);
			}
		});
		us.load(r);*/
		//var s: Sound = new Sound(r, new SoundLoaderContext(5000));
		//s.addEventListener(Event.ID3, function():void{
		//	t.text = 'dddddddddddddddd' + s.id3.artist;
		//});
		//s.play();
		//var nc: NetConnection = new NetConnection();
		//nc.connect(null);
		//var ns: NetStream = new NetStream( nc );
		//ns.soundTransform = new SoundTransform( 1 );
		//ns.play( "http://72.26.204.35/di_chillout_aacplus.flv?r=1298661081276" );
		//addEventListener(MouseEvent.CLICK, function():void{
		//	var str: String = '';
			//for( var i:String in s.id3 ){
			//	str += i + ": " + s.id3[i] + '; ';
			//}
		//	t.text = 'dd' + Math.random() + str;
		//});
	}
}

}