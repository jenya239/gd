package com.turbostool.client.net
{
import com.turbostool.client.event.EventManager;
import com.turbostool.client.event.ServerConnectErrorEvent;
import com.turbostool.client.net.messages.CarStateDataMessage;
import com.turbostool.client.net.messages.PingMessage;
import com.turbostool.client.net.messages.ServerRequest;
import com.turbostool.client.net.messages.ServerResponseEvent;
import com.turbostool.client.net.messages.ServerTimeMessage;
import com.turbostool.client.utils.Utils;

import mx.logging.ILogger;
import mx.logging.Log;
import mx.managers.PopUpManager;

import flash.net.*;
import flash.events.*;
import flash.utils.*;
import flash.system.*;
import flash.errors.*;
import com.turbostool.client.*;

public class SessionSocket extends Socket
{
    public static const DISCONNECTED:String = 'disconnected';
    public static const SESSION_INFO_WAITING:String = 'sessionInfoWaiting';

    private static const PING_INTERVAL: int = 10000;

    private var myReader:MessageReader;
    private var myWriter:MessageWriter;
    private var myLogger:ILogger;
    //    private var myLocalSession:LocalSession;
    //    private var myRemoteSessions:Array;
    private var myHost:String;
    private var myPort:int;
    private var myPolicyPort:int;

    private var myBuffer:ByteArray;
    private var myMsgLength: int;

    private static var _cookie: SharedObject = SharedObject.getLocal("GDClientCookie");

    private static var ourInstanceProperty: SessionSocket;

    public static function get instance(): SessionSocket
    {
        if (ourInstanceProperty == null)
        {
            ourInstanceProperty = new SessionSocket();
        }
        return ourInstanceProperty;
    }

    public function SessionSocket()
    {
        super();
        myReader = new MessageReader();
        myWriter = new MessageWriter();
        myLogger = Log.getLogger('SessionSocket');
        myLogger.info('sessionSocket created');
        myBuffer = new ByteArray();
        myMsgLength = -1;
        configureListeners();
        EventManager.globalChannel.addEventListener(ServerTimeMessage.SERVER_TIME, onServerTime);
        this.addEventListener(Event.CLOSE, onClose);
        setInterval(onPingTimer, PING_INTERVAL);
        setInterval(onSpeedMeasureTimer, _speedMeasureIntervalInSecs * 1000);
    }

    private function onClose(e: Event): void
    {
        EventManager.instance.dispatchEvent(e);
        var connectionLostPopup: ConnectionLostPopup = new ConnectionLostPopup();
        PopUpManager.addPopUp(connectionLostPopup, Client.instance, true);
        PopUpManager.centerPopUp(connectionLostPopup);
    }

    private function onPingTimer(): void
    {
        sendMessage(new PingMessage());
    }

    private function onServerTime(e: ServerResponseEvent): void
    {
        var timeMessage: ServerTimeMessage = ServerTimeMessage(e.response);

        var now: Number = new Date().time;
        var serverTime: Number = timeMessage.time;
        var d: Number = serverTime - now;
        //trace("serverTime = " + serverTime + ", now = " + now + ", d = " + d + "message = " + timeMessage.adv);
        Client.instance.startupInfoMessage = timeMessage.adv;
        Utils.serverDeltaTime = d;
    }

    private function configureListeners():void
    {
        addEventListener(Event.CLOSE, closeHandler);
        addEventListener(Event.CONNECT, connectHandler);
        addEventListener(IOErrorEvent.IO_ERROR, ioErrorHandler);
        addEventListener(SecurityErrorEvent.SECURITY_ERROR, securityErrorHandler);
        addEventListener(ProgressEvent.SOCKET_DATA, socketDataHandler);

        //	    addEventListener(SessionDataEvent.ADD_SESSION, addSessionHandler);
        //	    addEventListener(SessionDataEvent.REMOVE_SESSION, removeSessionHandler);
        //	    addEventListener(VarDataEvent.VAR_DATA, varDataHandler);

        //addEventListener(ServerConnectedEvent.SERVER_CONNECTED, logicalConnectedHandler);
        //myLocalSession.addEventListener(SetVarEvent.SET_VAR, setLocalVarHandler);
    }

    public function readFullUTF():String
    {
        return readUTFBytes(bytesAvailable);
    }

    private function closeHandler(event:Event):void
    {
        myLogger.info('Socket disconnected');
    }

    private function connectHandler(event:Event):void
    {
        myLogger.info('Socket connected');

        //todo may be dont duplicate?
        EventManager.instance.dispatchEvent(new ServerConnectedEvent());
        dispatchEvent(new ServerConnectedEvent());
    }

    private function ioErrorHandler(event:IOErrorEvent):void
    {
        myLogger.error('Socket io error ({0})', event);

        //todo may be dont duplicate?
        EventManager.instance.dispatchEvent(new ServerConnectErrorEvent(event));
        dispatchEvent(new ServerConnectErrorEvent(event));
    }

    private function securityErrorHandler(event:SecurityErrorEvent):void
    {
        myLogger.error('Socket security error ({0})', event);

        //todo may be dont duplicate?
        EventManager.instance.dispatchEvent(new ServerConnectErrorEvent(event));
    }

    public function byteArrayDebugString(bytes: ByteArray): String
    {
        var res: String = "";
        for (var i: int = 0; i < bytes.length; i++)
        {
            if (res.length > 0) res += ", ";
            res += bytes[i].toString();
        }
        return res;
    }

    private function socketDataHandler(event: ProgressEvent): void
    {
        readBytes(myBuffer, myBuffer.length);
        myBuffer.position = 0;

        while (true)
        {
            if (myMsgLength == -1)
            {
                if (myBuffer.bytesAvailable < 4)
                {
                    break;
                }
                //trace(byteArrayDebugString(myBuffer));
                myMsgLength = myBuffer.readUnsignedInt();
            }
            if (myBuffer.bytesAvailable < myMsgLength)
            {
                break;
            }

            var bytes:ByteArray = new ByteArray();
            myBuffer.readBytes(bytes, 0, myMsgLength);

            _messageInCount++;
            _bytesInCount += bytes.length;

            myMsgLength = -1;
            try
            {
                var ev: ServerResponseEvent = myReader.read(bytes);
                if (ev != null)
                {
                    if (!(ev is CarStateDataMessage))
                    {
                        //myLogger.debug("Получено сообщение: " + ev.toString());
                    }

                    EventManager.instance.dispatchEvent(ev);
                }
                else
                {
                    myLogger.debug("Получено неизвестное сообщение");
                }
            }
            catch (e:MessageConvertError)
            {
                myLogger.error('Incorrect message accepted ({0})', e.message);
                //todo should we send message to server is case of error?
                //sendMessage(ErrorDataEvent.createMessage(ErrorDataEvent.INCORRECT_BYTES));
            }
        }
        var newBuffer: ByteArray = new ByteArray();
        myBuffer.readBytes(newBuffer);
        myBuffer = newBuffer;
    }

    public function addDataHandler(listener:Function):void
    {
        addEventListener(ProgressEvent.SOCKET_DATA, listener);
    }

    private const _speedMeasureIntervalInSecs: int = 3;
    private var _messageInCount: int = 0;
    private var _messageOutCount: int = 0;
    private var _bytesInCount: int = 0;
    private var _bytesOutCount: int = 0;
    private var _speedInBytesPerSec: Number = 0;
    private var _speedOutBytesPerSec: Number = 0;

    private var _lastBytesInCount: int = 0;
    private var _lastBytesOutCount: int = 0;

    public function get messageInCount(): int
    {
        return _messageInCount;
    }

    public function get messageOutCount(): int
    {
        return _messageOutCount;
    }

    public function get bytesInCount(): int
    {
        return _bytesInCount;
    }

    public function get bytesOutCount(): int
    {
        return _bytesOutCount;
    }

    public function get speedInBytesPerSec(): Number
    {
        return _speedInBytesPerSec;
    }

    public function get speedOutBytesPerSec(): Number
    {
        return _speedOutBytesPerSec;
    }

    private function onSpeedMeasureTimer(): void
    {
        var deltaInBytes: int = _bytesInCount - _lastBytesInCount;
        var deltaOutBytes: int = _bytesOutCount - _lastBytesOutCount;

        _speedInBytesPerSec = deltaInBytes / _speedMeasureIntervalInSecs;
        _speedOutBytesPerSec = deltaOutBytes / _speedMeasureIntervalInSecs;

        _lastBytesInCount = _bytesInCount;
        _lastBytesOutCount = _bytesOutCount;
    }

    public function sendMessage(message: ServerRequest):void
    {
        try
        {

            var bytes:ByteArray = myWriter.createBytes(message);
            if (!(message is CarStateDataMessage))
            {
                //myLogger.info('Попытка отослать: {0} , длинной {1}', message.toString(), bytes.length);
            }
            writeInt(bytes.length);
            writeBytes(bytes);
            flush();
            _messageOutCount++;
            _bytesOutCount += bytes.length;
        }
        catch (e:IOError)
        {
            myLogger.error('Не удалось отослать {0}: {1}', message.toString(), e.message);
        }
    }

    public override function connect(host: String, port: int):void
    {
        myHost = host;
        myPort = port;
        try{
            super.connect(host, port);// socket.close()
            myLogger.error('удалось приконнектиться к серверу хост {0} порт {1}', host, port);
        }catch(e:*){
            myLogger.error('не удалось приконнектиться к серверу хост {0} порт {1}', host, port);
        }
    }

    public function connect2(host: String, port: int, policyPort: int):void
    {
        myPolicyPort = policyPort;
        var policyFileURL: String = "xmlsocket://" + host + ":" + policyPort;
        Security.loadPolicyFile(policyFileURL);
        connect(host, port);
    }

    public function getHost():String
    {
        return myHost;
    }

    public function getPort():int
    {
        return myPort;
    }

    public function reconnect(): void
    {
        if (connected)
        {
            super.close();
        }
        super.connect(myHost, myPort);
    }
}
}