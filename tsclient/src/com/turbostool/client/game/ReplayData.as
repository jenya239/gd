package com.turbostool.client.game
{
import com.turbostool.client.game.components.car.CarControlEvent;
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.Utils;

import mx.utils.StringUtil;

public class ReplayData {

    public static const MODE_OFF: String = "off";
    public static const MODE_ON: String = "on";
    public static const MODE_DEBUG: String = "debug";

    private static const REPLAY_SIGNATURE: String = "Replay: ";
    private var _records: Array = new Array();
    private const MAX_RECORDS: int = 2000;
    private var _lastEvent: CarControlEvent = null;
    private var _lastPosInfo: ReplayPositionInfo = null;
    private var _nextIndex: int = 0;

    private static const PARSE_STATE_0_TOKEN: int = 0;
    private static const PARSE_STATE_1_IN_BRACKETS: int = 1;
    private static const PARSE_STATE_2_EOS: int = 2;

    private static const PARSE_EVENT_0_COMMA: int = 0;
    private static const PARSE_EVENT_1_OPEN_BRACKET: int = 1;
    private static const PARSE_EVENT_2_CLOSE_BRACKET: int = 2;
    private static const PARSE_EVENT_3_EOS: int = 3;
    private static const PARSE_EVENT_4_OTHER: int = 4;

    private var _targetIndex: int = -1;
    private var _curDT: Number = -1;
    private var _curEvent: CarControlEvent = null;
    private var _curPosInfo: ReplayPositionInfo = null;

    public function ReplayData() {
    }

    public function isFull(): Boolean {
        return _records.length >= MAX_RECORDS;
    }

    public function add(dt: Number, event: CarControlEvent, posInfo: ReplayPositionInfo): void {
        if (isFull()) {
            throw new TSError("Replay data is full");
        }

        if (event != null && !event.equals(_lastEvent)) {
            _lastEvent = event;
        } else {
            event = null;
        }

        if (posInfo != null && !posInfo.equals(_lastPosInfo)) {
            _lastPosInfo = posInfo;
        } else {
            posInfo = null;
        }

        _records.push(new ReplayDataRecord(dt, event, posInfo));
    }

    public function hasNext(): Boolean {
        return _nextIndex < _records.length;
    }

    public function getNext(): ReplayDataRecord {
        if (!hasNext()) {
            throw new TSError("Replay doesn't have more records");
        }
        var rec: ReplayDataRecord = _records[_nextIndex];
        _nextIndex++;
        return rec;
    }

    public static function checkSignature(str: String): Boolean {
        return Utils.stringStartsWith(str, REPLAY_SIGNATURE);
    }

    public function clear(): void {
        _lastEvent = null;
        _records = new Array();
        reset();
    }

    public function reset(): void {
        _nextIndex = 0;
    }

    public static function decode(str: String): ReplayData {
        var replay: ReplayData = new ReplayData();
        replay.decode(str);
        return replay;
    }

    public function get Count(): int {
        return _records.length;
    }

    public function encode(): String {
        var str: String = "";
        while (hasNext()) {
            if (str.length > 0) {
                str += ',';
            }
            var rec: ReplayDataRecord = getNext();
            str += rec.encode();
        }
        return REPLAY_SIGNATURE + str;
    }

    public function decode(str: String): void {
        clear();
        if (!Utils.stringStartsWith(str, REPLAY_SIGNATURE)) {
            throw new TSError("Invalid replay signature");
        }

        str = str.substr(REPLAY_SIGNATURE.length);

        var state: int = PARSE_STATE_0_TOKEN;
        var i: int = 0;
        var token: String = "";
        while (state != PARSE_STATE_2_EOS) {
            var event: int;
            var char: String;
            var oldState: int = state;

            if (i >= str.length) {
                event = PARSE_EVENT_3_EOS;
            } else {
                char = str.charAt(i);
                switch (char) {
                    case ',':
                        event = PARSE_EVENT_0_COMMA;
                        break;
                    case '[':
                        event = PARSE_EVENT_1_OPEN_BRACKET;
                        break;
                    case ']':
                        event = PARSE_EVENT_2_CLOSE_BRACKET;
                        break;
                    default:
                        event = PARSE_EVENT_4_OTHER;
                        break;
                }
            }

            i++;

            switch (state) {
                case PARSE_STATE_0_TOKEN:
                    switch (event) {
                        case PARSE_EVENT_0_COMMA:
                            parseDT(token);
                            token = "";
                            break;
                        case PARSE_EVENT_3_EOS:
                            parseDT(token);
                            addRecord();
                            token = "";
                            state = PARSE_STATE_2_EOS;
                            break;
                        case PARSE_EVENT_1_OPEN_BRACKET:
                            parseDT(token);
                            token = "";
                            state = PARSE_STATE_1_IN_BRACKETS;
                            break;
                        default:
                            token += char;
                            break;
                    }
                    break;
                case PARSE_STATE_1_IN_BRACKETS:
                    switch (event) {
                        case PARSE_EVENT_2_CLOSE_BRACKET:
                            parseClass(token);
                            token = "";
                            state = PARSE_STATE_0_TOKEN;
                            break;
                        case PARSE_EVENT_3_EOS:
                            parseClass(token);
                            addRecord();
                            token = "";
                            state = PARSE_STATE_2_EOS;
                            break;
                        default:
                            token += char;
                            break;
                    }
                    break;
            }
            //				if (state != oldState)
            //					trace("" + oldState + " => " + state + " '" + char + "'");
        }
    }

    private function addRecord(): void {
        if (_curDT > 0) {
            var rec: ReplayDataRecord = new ReplayDataRecord(_curDT, _curEvent, _curPosInfo);
            //				trace(rec.encode());
            _records.push(rec);

            _curEvent = null;
            _curPosInfo = null;
        }
    }

    private function parseDT(token: String): void {
        token = StringUtil.trim(token);
        if (token.length == 0)
            return;

        //trace("parseDT(" + token + ")");
        addRecord();

        _curDT = parseFloat(token);
    }

    private function parseClass(token: String): void {
        //trace("parseClass(" + token + ")");
        if (token.length < 2) {
            throw new TSError("Unable to parse replay class");
        }

        const CLASS_ID_LENGTH: int = 2;
        var classID: String = token.substr(0, CLASS_ID_LENGTH);
        var obj: Object;
        switch (classID) {
            case 'e:':
                _curEvent = CarControlEvent.decode(token.substr(CLASS_ID_LENGTH));
                break;
            case 'p:':
                _curPosInfo = ReplayPositionInfo.decode(token.substr(CLASS_ID_LENGTH));
                break;
            default:
                throw new TSError("Unknown replay class ID: " + classID);
        }
    }

    public function get nextIndex(): int {
        return _nextIndex;
    }

    public function get targetIndex(): int {
        return _targetIndex;
    }

    public function set targetIndex(value: int): void {
        _targetIndex = (value <= _records.length) ? value : _records.length - 1;
    }

    public function canPlay(): Boolean {
        return _targetIndex < 0 || _nextIndex <= _targetIndex;
    }
}
}