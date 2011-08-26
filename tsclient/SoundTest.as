package {
import flash.display.Sprite;
import flash.media.Sound;
import flash.text.*;
import flash.utils.Timer;
import flash.events.*;

public class SoundTest extends Sprite {

		//sound experiment
		import flash.media.*;
		[Embed(source="/src/assets/sounds/1000rpm2.mp3")]
		[Bindable]
		private var rpm1000Cls:Class;
		[Embed(source="/src/assets/sounds/2000rpm.mp3")]
		[Bindable]
		private var rpm2000Cls:Class;
		[Embed(source="/src/assets/sounds/3000rpm.mp3")]
		[Bindable]
		private var rpm3000Cls:Class;
		[Embed(source="/src/assets/sounds/4000rpm.mp3")]
		[Bindable]
		private var rpm4000Cls:Class;
		[Embed(source="/src/assets/sounds/6000rpm.mp3")]
		[Bindable]
		private var rpm6000Cls:Class;
		private var sounds:Array = new Array(
			{rpm:1000, snd:new rpm1000Cls()},
			{rpm:2000, snd:new rpm2000Cls()},
			{rpm:3000, snd:new rpm3000Cls()},
			{rpm:4000, snd:new rpm4000Cls()},
			{rpm:60000, snd:new rpm6000Cls()}
		);
		private var sndChannel:SoundChannel = null;
		private var oldChannel:SoundChannel = null;
		private var currentSound: Sound = null;
		private static const SHIFT: int = 200;
		private var stopTimer: Timer = null;
		private var loopTimer: Timer = null;
	  private function soundExperiment(rpm: Number): void{
	//	так-вот-вот-са.-эксп.-с рпм тогда
	//  какой должен бы играть
	//  какой играет
	//  если не совпадает то
	//     запускаем таймер на остановку текущего
	//     начинаем новый звук
	//     перезапускаем таймер на продолжение
			if( stopTimer == null ){
				stopTimer = new Timer(SHIFT, 1);
				stopTimer.addEventListener(TimerEvent.TIMER, function():void{
					oldChannel.stop();
				})
			}
			if( loopTimer == null ){
				loopTimer = new Timer(1000);
				loopTimer.addEventListener(TimerEvent.TIMER, function():void{
					oldChannel = sndChannel;
					sndChannel = currentSound.play();
				})
			}
			var needSound: Sound;
			for( var i:int = 0; i < sounds.length; i++ ){
				if( rpm < sounds[i].rpm ){
					needSound = sounds[i].snd;
					break;
				}
			}
			if( needSound != currentSound ){
				if( currentSound != null ){
					stopTimer.start();
				}
				oldChannel = sndChannel;
				currentSound = needSound;
				sndChannel = currentSound.play();
				loopTimer.stop();
				loopTimer.delay = currentSound.length - SHIFT;
				loopTimer.start();
			}
		}
		//end sound experiment

	public function SoundTest(){
//		var tf:TextField = new TextField();
//		tf.text = "sd";
//		addChild(tf);
//		var chan: SoundChannel;
//		var sound: Sound = sounds[0].snd;
//		var resound: Function = function():void{
//			chan.removeEventListener(Event.SOUND_COMPLETE, resound);
//			//chan = sound.play();
//			//chan.addEventListener(Event.SOUND_COMPLETE, resound);
//			tf.text += "ssss\n";
//		};
//		chan = sound.play();
//		chan.addEventListener(Event.SOUND_COMPLETE, resound);
//		//
//		var timer: Timer = new Timer(sound.length - 600);
//		timer.start();
//		timer.addEventListener(TimerEvent.TIMER, function():void{
//			tf.text += "" + sound.length;
//			sound.play();
//		});
		//soundExperiment(900);
		//(sounds[1].snd as Sound).play(0, 9999);
		var mp: MP3Pitch = new MP3Pitch('');
		mp.rate = 3.0;
		//var mySound:Sound = new Sound();
		//function sineWaveGenerator(event:SampleDataEvent):void {
		//		for ( var c:int=0; c<8192; c++ ) {
		//				event.data.writeFloat(Math.sin((Number(c+event.position)/Math.PI/2))*0.25);
		//				event.data.writeFloat(Math.sin((Number(c+event.position)/Math.PI/2))*0.25);
		//		}
		//}
		//mySound.addEventListener(SampleDataEvent.SAMPLE_DATA,sineWaveGenerator);
		//mySound.play();
		
	}
}

}