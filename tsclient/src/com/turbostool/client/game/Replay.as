package com.turbostool.client.game
{
import com.turbostool.client.Config;
import com.turbostool.client.utils.Utils;

import flash.events.Event;
import flash.events.MouseEvent;
import flash.events.TextEvent;
import flash.system.System;

import mx.controls.Alert;

public class Replay
{

    protected var myEngine:GameEngine;

    public function Replay(engine:GameEngine)
    {
        myEngine = engine;
        GameGUI.mybtReplayStep.visible = Config.instance.replayDebugEnabled;
        GameGUI.mybtReplayStep10.visible = Config.instance.replayDebugEnabled;
        GameGUI.mybtReplayStep100.visible = Config.instance.replayDebugEnabled;

        GameGUI.mytiReplay.addEventListener(TextEvent.TEXT_INPUT, onReplayTextInput);
        GameGUI.mybtReplayStep.addEventListener(MouseEvent.CLICK, onReplayStep);
        GameGUI.mybtReplayStep10.addEventListener(MouseEvent.CLICK, onReplayStep10);
        GameGUI.mybtReplayStep100.addEventListener(MouseEvent.CLICK, onReplayStep100);
        myEngine.addEventListener("state_changed", onEngineStateChanged);
        myEngine.addEventListener("replayIndex_changed", onReplayIndexChanged);
        myEngine.addEventListener("replay_full", onReplayFull);
    }

    public function onReplayIndexChanged(event: Event):void {
        GameGUI.mylbReplayIndex.text = " " + Utils.formatTime(myEngine.replayTime * 1000) + " " + (myEngine.replay.nextIndex - 1);
    }

    public function onEngineStateChanged(event: Event):void {
        //trace("engine state changed: " + myEngine.State);
        GameGUI.myimgRec.visible = false;
        GameGUI.myimgPlay.visible = false;
        GameGUI.mylbReplayIndex.visible = false;
        switch (myEngine.state) {
            case GameEngine.STATE_RECORD:
                GameGUI.myimgRec.visible = true;
                break;
            case GameEngine.STATE_REPLAY:
                GameGUI.myimgPlay.visible = true;
                GameGUI.mylbReplayIndex.visible = true;
                break;
        }
    }

    public function onReplayStep(event: MouseEvent):void {
        myEngine.replay.targetIndex = myEngine.replay.nextIndex - 1 + 1;
    }

    public function onReplayStep10(event: MouseEvent):void {
        myEngine.replay.targetIndex = myEngine.replay.nextIndex - 1 + 10;
    }

    public function onReplayStep100(event: MouseEvent):void {
        myEngine.replay.targetIndex = myEngine.replay.nextIndex - 1 + 100;
    }

    public function onReplayTextInput(event: TextEvent):void {

        if (ReplayData.checkSignature(event.text)) {
            //GameGUI.mytiReplay.text = event.text;
            event.preventDefault();
            myEngine.replay.decode(event.text);
        } else {
            event.preventDefault();
            if (event.text.length > 1) {
                Alert.show("Invalid replay", "Error", Alert.OK);
            }
        }
    }

    public function onReplayFull(event: Event):void {
        copyReplayToClipboard();
    }

    public function copyReplayToClipboard(): void {
        var s: String = myEngine.replay.encode();
        System.setClipboard(s);
    }

}
}