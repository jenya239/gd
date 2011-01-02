package com.turbostool.client.game
{
import mx.containers.Canvas;
import mx.controls.*;
import mx.core.Application;

public class GameGUI
{
    public function GameGUI()
    {
    }

    public static function get myBReload():Button {
        return Application.application['bReload'];
    }

    public static function get myTiMessage():TextInput {
        return Application.application['tiMessage'];
    }

    public static function get myPBLoadingProgress(): ProgressBar {
        var app: Application = Application.application as Application;
        return app['pbLoadingProgress'] as ProgressBar;
    }

    public static function get myCLoadingCanvas(): Canvas {
        var app: Application = Application.application as Application;
        return app['cLoadingCanvas'] as Canvas;
    }

    public static function get mytiReplay(): TextInput {
        return Application.application.gs['tiReplay'];
    }

    public static function get myimgRec(): Image {
        return Application.application.gs['imgRec'];
    }

    public static function get myimgPlay(): Image {
        return Application.application.gs['imgPlay'];
    }

    public static function get mybtReplayStep(): Button {
        return Application.application.gs['btReplayStep'];
    }

    public static function get mybtReplayStep10(): Button {
        return Application.application.gs['btReplayStep10'];
    }

    public static function get mybtReplayStep100(): Button {
        return Application.application.gs['btReplayStep100'];
    }

    public static function get mylbReplayIndex(): Label {
        return Application.application.gs['lbReplayIndex'];
    }
}
}