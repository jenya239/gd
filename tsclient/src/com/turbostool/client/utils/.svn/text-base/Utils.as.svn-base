package com.turbostool.client.utils {
import com.turbostool.client.ModelsStorage;
import com.turbostool.client.Tracker;
import com.turbostool.client.model.CarInfo;
import com.turbostool.client.model.ItemInfo;
import com.turbostool.client.net.SessionSocket;

import com.turbostool.client.net.messages.ChangeTriggerMessage;

import flash.utils.getQualifiedClassName;

import mx.controls.Alert;
import mx.utils.StringUtil;

public class Utils {
    //todo: maybe 10^-6 will be enough?
    public static const EPSILON: Number = 0.000000000001;
    public static const G: Number = 9.8;
    public static const AIR_DENSITY: Number = 1.43;
    public static var serverDeltaTime: Number = 0;

    public static function getSimpleClassName(o: Object): String {
        var name: String = getQualifiedClassName(o);
        return name.substring(name.indexOf("::") + 2);
    }

    public static function equal(n1: Number, n2: Number): Boolean {
        //return Math.abs(n1 - n2) < EPSILON;
        return n1 - n2 < EPSILON && n2 - n1 < EPSILON;
    }

    public static function nonEqual(n1: Number, n2: Number): Boolean {
        return Math.abs(n1 - n2) > EPSILON;
    }

    public static function isZero(n: Number): Boolean {
        return equal(0, n);
    }

    public static function nonZero(n: Number): Boolean {
        return !equal(0, n);
    }

    public static function inRange(value: Number, left: Number, right: Number): Boolean {
        return (value >= left) && (value <= right);
    }

    public static function isBetween(value: Number, num1: Number, num2: Number): Boolean {
        return inRange(value, num1, num2) || inRange(value, num2, num1);
    }

    public static function getNear(value: Number, num1: Number, num2: Number): Number {
        var left: Number = (num1 <= num2) ? num1 : num2;
        var right: Number = (num1 <= num2) ? num2 : num1;
        var res: Number = value;
        if (value < left) res = left;
        if (value > right) res = right;
        return res;
    }

    public static function sign(value: Number): int {
        if (Utils.equal(value, 0)) return 0;
        if (value > 0) return 1;
        return -1;
    }

    //The number of milliseconds since midnight January 1, 1970, universal time,
    public static function now(): Number {
        var d: Date = new Date();
        return d.time + serverDeltaTime;
    }

    public static function levelString(s: String, len: int): String {
        s.substr(0, len);
        while (s.length < len) {
            s += ' ';
        }
        return s;
    }

    public static function sqr(n: Number): Number {
        return n * n;
    }

    /**
     * @throws TSError если главный определитель равен 0
     */
    public static function solveSystem2d(matrix: Matrix2d, right: Vector2d): Vector2d {
        var delta: Number = matrix.determinant();
        if (isZero(delta)) {
            throw new TSError('Главный определитель ' + matrix + 'равен нулю');
        }
        delta = 1 / delta;
        var deltaX: Number = right.myX * matrix.myA22 - right.myY * matrix.myA12;
        var deltaY: Number = matrix.myA11 * right.myY - matrix.myA21 * right.myX;
        return new Vector2d(deltaX * delta, deltaY * delta);
    }

    public static function isTurning(equilibrium: Number, oldCoord: Number,
                                     currentCoord: Number, newVelocity: Number): Boolean {
        return isBetween(equilibrium, oldCoord, currentCoord)
                && (sign(currentCoord - oldCoord) * sign(newVelocity) < 0);
    }

    public static function isTurning2(equilibrium: Number, oldOld: Number, old: Number, curr: Number): Boolean {
        return isBetween(equilibrium, oldOld, old) && isBetween(equilibrium, old, curr)
                && !equal(oldOld, old);
    }

    public static function scalarProduct(r1: Array, r2: Array):Number {
        Assert.assertEquals(r1.length, r2.length);
        var sum: Number = 0;
        for (var i: int = 0; i < r1.length; i++) {
            sum += r1[i] * r2[i];
        }
        return sum;
    }

    public static function pow2(n: int): int {
        return (1 << n);
    }

    /**
     * Пересекает ли гиперплоскость ai * xi = b параллелепипед [-xiMax, xiMax]
     */
    public static function giperIntersects(a: Array, b:Number, xMax: Array): Boolean {
        function plusOne(r: Array):void {
            for (var j: int = 0; j < r.length; j++) {
                if (r[j] < 0) {
                    r[j] = - r[j];
                    return;
                } else if (r[j] > 0) {
                    r[j] = - r[j];
                }
                if (r[j] == 0) {
                    throw new TSError();
                }
            }
            //Assert.fail();
        }

        var i: int;
        Assert.assertEquals(a.length, xMax.length);
        Assert.assertTrue(a.length > 0);
        var firstNonZeroSign: int = 0;
        var r: Array = new Array(xMax.length);
        for (i = 0; i < xMax.length; i++) {
            r[i] = (xMax[i] == 0) ? Utils.EPSILON * 2 : - xMax[i];
        }
        for (i = 0; i < pow2(a.length); i++) {
            var sign: int = sign(scalarProduct(a, r) - b);
            if (sign == 0) {
                return true;
            } else if (firstNonZeroSign == 0) {
                firstNonZeroSign = sign;
            } else if (firstNonZeroSign != sign) {
                return true;
            }
            plusOne(r);
        }
        return false;
    }

    //производная непрерывна
    public static function getBumpless(x0:Number, xe:Number, v0:Number, te:Number, dt: Number):Array {
        Assert.assertTrue(te > 0);
        Assert.assertTrue(inRange(dt, 0, te));
        var dx: Number = 2 * (x0 - xe);
        var sD: Number = Math.sqrt(sqr(dx + te * v0) + sqr(te * v0));
        var a: Number = (v0 > 0)
                ? (- (dx + te * v0) + sD) / sqr(te)
                : (- (dx + te * v0) - sD) / sqr(te);
        var tc: Number = (te - v0 / a) / 2;
        var res: Array = new Array(); //[x(dt), v(dt)]
        if (dt < tc) {
            res[0] = x0 + v0 * dt + a * sqr(dt) / 2;
            res[1] = v0 + a * dt;
        } else {
            res[0] = xe + a * te * dt - a * (sqr(te) + sqr(dt)) / 2;
            res[1] = a * (te - dt);
        }
        return res;
    }

    public static function round(x: Number, countOfDigit: int = 0): Number {
        return Math.round(x * Math.pow(10, countOfDigit)) / Math.pow(10, countOfDigit);
    }

    public static function roundToGreater(n: Number): int
    {
        var nInt: int = (int(n));
        return Math.abs(n - nInt) > 0.01 ? nInt + 1 : nInt;
    }

    public static function str2bool(s: String): Boolean {
        s = StringUtil.trim(s);
        if (s.toLowerCase() == 'true') {
            //Alert.show(s+"["+s.length+"]");
            return true;
        } else if (s.toLowerCase() == 'false') {
            //Alert.show(s+"["+s.length+"]");
            return false;
        } else {
            throw new TSError("can't convert [" + s + "] to Boolean");
        }
    }

    public static function numberToString(value: Number, countOfDigit: int, precision: int): String {
        var realCount: int = Math.ceil(Math.log(value) / Math.LN10);
        value = round(value, precision);
        var stringNumber: String = "" + value;
        var minus: int = value < 0 ? 1 : 0;
        if (realCount + minus < countOfDigit) {
            for (var i: int = 0; i < countOfDigit - realCount - minus; i++) {
                stringNumber = " " + stringNumber;
            }
        }
        for (var j: int = 0; j < countOfDigit + precision + 1; j++) {
            stringNumber = stringNumber + " ";
        }
        return stringNumber;
    }

    public static function boolToFixedString(bool: Boolean): String {
        return bool ? "true " : "false";
    }

    public static function boolToString(bool: Boolean): String {
        return bool ? "true" : "false";
    }

    public static function formatNumber(num: Number): String
    {
        var i: int = Math.floor(num * 100);
        var i1: int = int(i / 100);
        var i2: int = i % 100;
        return "" + i1 + "." + (i2 >= 10 ? "" + i2 : "0" + i2);
    }

    public static function formatTime(time: Number): String {
        var min: int = Math.floor(time / 60000);
        var sec: int = Math.floor((time - min * 60000) / 1000);
        var ss: String = (sec < 10) ? '0' + sec : sec.toString();
        var cent: int = Math.floor((time - min * 60000 - sec * 1000) / 10);
        var cs: String = (cent < 10) ? '0' + cent : cent.toString();
        var s: String;
        return min + ':' + ss + ':' + cs;
    }

	public static function formatSignedValue(val: Number, unit: String = ""): String{
		val = Math.round(val);
		if( isNaN(val) || Utils.isZero(val) ) return "-";
		if( val > 0 ) return "+" + val.toString() + unit;
		return val.toString() + unit;
	}

    public static function stringStartsWith(str: String, head: String): Boolean {
        return str.length >= head.length && str.substr(0, head.length) == head;
    }

    public static function arrayToString(arr: Array, delim: String): String {
        var result: String = "";
        for each(var elem: String in arr) {
            if (result.length > 0) {
                result += delim;
            }
            result += elem;
        }
        return result;
    }

    // скопирован из UIComponent.as
    public static function getClassName(obj: Object):String
    {
        var name:String = getQualifiedClassName(obj);

        // If there is a package name, strip it off.
        var index:int = name.indexOf("::");
        if (index != -1)
            name = name.substr(index + 2);

        return name;
    }

    // по образу и подобию LogLogger.debug(..)
    //todo: оптимизировать
    public static function stringFormat(str: String, ... rest): String
    {
        // replace all of the parameters in the msg string
        for (var i: int = 0; i < rest.length; i++)
        {
            str = str.replace(new RegExp("\\{" + i + "\\}", "g"), rest[i]);
        }
        return str;
    }

    public static function contains(array: Array, item: *): Boolean {
        for (var i: * in array)
        {
            if (i == item) return true;
        }
        return false;
    }

    public static function remove(array: Array, item: *): void {
        for (var i: int = 0; i < array.length; i++) {
            if (array[i] == item) array.splice(i, 1);
        }
    }

    public static function formatCountdown(timerEnd: int): String {
        var timeLeft: Number = timerEnd - now();
        if (timeLeft < 0) timeLeft = 0;
        return formatTime(timeLeft);
    }

    public static function insertElement(array: Array, obj: Object): Array
    {
        var newArray: Array = new Array();
        newArray.push(obj);
        return newArray.concat(array);
    }

    public static function formatPrice(money:Number, isGold: Boolean = false, digits: int = 0): String
    {
        var s: String = Utils.round(money, digits) + (isGold ? Client.instance.str("goldSign") : Client.instance.str("creditsSign"));
        return s;
    }

    public static function nextTutorialStage(): void {
        SessionSocket.instance.sendMessage(new ChangeTriggerMessage(ChangeTriggerMessage.TUTORIAL_STAGE, 1));

        if(Client.instance.modelsStorage.userInfo.tutorialStage >= 5) {
            Tracker.instance.trackEvent("tutorial", "complete");
            Tracker.instance.trackPageview("/tutorial/complete");
        } else {
            Tracker.instance.trackEvent("tutorial", "nextStage", "stage" + Client.instance.modelsStorage.userInfo.tutorialStage);
            Tracker.instance.trackPageview("/tutorial/stage" + Client.instance.modelsStorage.userInfo.tutorialStage);
        }
    }

    public static function calcSellPrice(itemInfo: ItemInfo, modelsStorage: ModelsStorage, isBuying: Boolean, digits: int = 0): String
    {
        if (itemInfo != null)
        {
            if (isBuying)
                return itemInfo.realPrice > 0 ? Utils.formatPrice(itemInfo.realPrice, true, digits) : Utils.formatPrice(itemInfo.price, false, digits);
            else
            {
                return Utils.formatPrice(itemInfo.sellPrice, false, digits);
            }
        }
        else
            return Utils.formatPrice(0, false);
    }

    public static function calcCarSellPrice(carInfo: CarInfo, modelsStorage: ModelsStorage, isBuying: Boolean, digits: int = 0): String
    {
        if(carInfo != null)
        {
            if(isBuying)
                return carInfo.realPrice > 0 ? Utils.formatPrice(carInfo.realPrice, true, digits) : Utils.formatPrice(carInfo.price, false, digits);
            else
            {
                return Utils.formatPrice(carInfo.sellPrice, false, digits);
            }
        }
        else
            return Utils.formatPrice(0, false);
    }
}
}