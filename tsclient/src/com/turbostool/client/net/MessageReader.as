package com.turbostool.client.net
{
import com.turbostool.client.model.CityInfo;
import com.turbostool.client.model.FriendInfo;
import com.turbostool.client.model.ItemInfo;
import com.turbostool.client.model.LobbyRaceResult;
import com.turbostool.client.model.PostMessageInfo;
import com.turbostool.client.model.RatingInfo;
import com.turbostool.client.model.UserDailyScoreInfo;
import com.turbostool.client.model.WearOutUpgrade;
import com.turbostool.client.model.WorkOfferInfo;
import com.turbostool.client.net.messages.*;
import com.turbostool.client.utils.Vector2d;

import flash.utils.*;

import mx.collections.ArrayCollection;
import mx.logging.*;

public class MessageReader
{
    private var myLogger: ILogger;

    public static const XML_TYPE: int = 128;
    public static const BINARY_TYPE: int = 64;

    public static const EVENT: String = "event";

    private static const quotRegExp: RegExp = new RegExp("#quot;", "g")
    private static const quotAndExp: RegExp = new RegExp("#and;", "g")

    public function MessageReader()
    {
        myLogger = Log.getLogger('MessageReader');
    }

    public function read(bytes: ByteArray): ServerResponseEvent
    {
        //trace(byteArrayDebugString(bytes));

        var event: ServerResponseEvent;
        if (bytes.bytesAvailable < 2)
        {
            throw new MessageConvertError('Слишком короткое сообщение: ' + bytes);
        }

        //var length:int = bytes.readShort();
        var first: int = bytes.readInt();

        if (first == BINARY_TYPE)
        {
            event = createCarStateDataEvent(bytes);
        }
        else
        {
            var xmlStr: String = bytes.readUTFBytes(bytes.bytesAvailable);
            event = createXMLEvent(xmlStr);
        }
        return event;
    }

    private function createXMLEvent(xmlStr: String): ServerResponseEvent
    {
        myLogger.debug(xmlStr);
        var xml: XML = new XML(xmlStr);

        if (xml.name().localName == EVENT)
        {
            return createEvent(xml);
        }
        else
        {
            throw new MessageConvertError("Неизвестный тэг xml: " + xml.name().localName);
        }
    }

    private function createCarStateDataEvent(bytesData: ByteArray): ServerResponseEvent
    {
        if (bytesData.bytesAvailable < 4)
        {
            throw new MessageConvertError('Message to short: ' + bytesData);
        }
        var sessionId:int = bytesData.readUnsignedInt();

        var r: Vector2d = new Vector2d(bytesData.readFloat(), bytesData.readFloat());
        var velocity: Vector2d = new Vector2d(bytesData.readFloat(), bytesData.readFloat());
        var angle: Number = bytesData.readFloat();
        var angularVelocity: Number = bytesData.readFloat();
        //        var lateralAngle: Number = bytesData.readFloat();
        //        var lateralAngularVelocity: Number = bytesData.readFloat();
        //        var longAngle: Number = bytesData.readFloat();
        //        var longAngularVelocity: Number = bytesData.readFloat();
        //        var rudderAngle: Number = bytesData.readFloat();
        //        var rudderAction: int = bytesData.readInt();
        //        var isAccelerate: Boolean = bytesData.readByte() != 0 ? true : false;
        //        var engineDroselCoef: Number = bytesData.readFloat();
        //        var isBrake: Boolean = bytesData.readByte() != 0 ? true : false;
        //        var brakeCoef: Number = bytesData.readFloat();
        //        var isHandBrake: Boolean = bytesData.readByte() != 0 ? true : false;

        var result: CarStateDataMessage = new CarStateDataMessage(sessionId, r, velocity, angle, angularVelocity) //, lateralAngle, lateralAngularVelocity, longAngle, longAngularVelocity, rudderAngle, rudderAction, isAccelerate, engineDroselCoef, isBrake, brakeCoef, isHandBrake);

        return new ServerResponseEvent(CarStateDataMessage.CAR_STATE_MESSAGE, result);
    }

    public static function toCamelCase(str: String): String
    {
        var firstLetter: String = str.charAt(0);
        var rest: String = str.substring(1);
        return firstLetter.toLocaleUpperCase() + rest;
    }

    private function createEvent(xml: XML): ServerResponseEvent
    {
        //trace(xml);
        //Alert.show(xml.toXMLString());
        var name: String = String(xml.attribute("name"));
        var message: Object;

        if (name == "get")
        {
            name = name + toCamelCase(xml.@property) + "Response";
            var className: String = "Get" + toCamelCase(xml.@property);
            message = tryToCreateInstanceByClassName("com.turbostool.client.net.messages", className + "Response");
            if (message == null)
            {
                message = new GetPropertyResponse();
            }

        }
        else
            if (name == "set")
            {
                message = new SetPropertyResponse();
                name = name + toCamelCase(xml.@property) + "Response";
            }
            else
            {
                var cameCaseName: String = toCamelCase(name);

                message = tryToCreateInstanceByClassName("com.turbostool.client.net.messages", cameCaseName + "Response");
                if (message == null)
                {
                    message = tryToCreateInstanceByClassName("com.turbostool.client.net.messages", cameCaseName + "Message");
                    if (message == null)
                    {
                        throw new MessageConvertError("Not found message for xml name '" + name + "'");
                    }
                }
            }

        fillMessageFromXml(xml, message);
        return new ServerResponseEvent(name, message);
    }

    private function fillMessageFromXml(xml: XML, message: Object): void
    {
        var property: Object;
        copyXmlAttributesToObject(xml, message, true);
        for each(var node: XML in xml.children())
        {
            if (node.localName() == "list")
            {
                message[node.@name] = createListFromXML(node);
            }
            else if (node.localName() == "property")
            {
                var child: XML = node.children()[0];
                property = createPropertyFromXml(child);
                message[node.@name] = property;
            }
            else
            {
                property = createPropertyFromXml(node);
                message[node.localName()] = property;
            }
        }
    }

    private function createPropertyFromXml(xml: XML): Object
    {
        // we need this in order the LobbyRaceResult got compiled 
        var lobbyRaceResultReference: LobbyRaceResult;
        var wearOutUpgrade: WearOutUpgrade;
        var ratingInfoReference: RatingInfo;
        var itemInfo: ItemInfo;
        var postMessageInfo: PostMessageInfo;
        var workOfferInfo: WorkOfferInfo;
        var friendInfo: FriendInfo;
        var cityInfo: CityInfo;
        var userDailyScoreInfo: UserDailyScoreInfo;

        var property: Object = tryToCreateInstanceByClassName("com.turbostool.client.model", toCamelCase(String(xml.localName())));
        if (property != null)
        {
            copyXmlAttributesToObject(xml, property, false);
            for each(var node: XML in xml.children())
            {
                if (node.localName() == "list")
                {
                    property[node.@name] = createListFromXML(node);
                }
                else if (node.localName() == "property")
                {
                    property[node.@name] = createPropertyFromXml(node);
                }
                else
                {
                    property[node.localName()] = createPropertyFromXml(node);
                }
            }

            return property;
        }
        else
        {
            throw new MessageConvertError("Can't create property '" + xml.localName() + "'");
        }
    }

    private function tryToCreateInstanceByClassName(packageName: String, className: String): Object
    {
        try
        {
            var fullClassName: String = packageName + "." + className;
            var objClass: Class = getDefinitionByName(fullClassName) as Class;
            return new objClass();
        }
        catch(e: Error)
        {
            //trace("Failed creating instance by class name: " + e.message);
        }
        return null;
    }

    private function copyXmlAttributesToObject(xml: XML, object: Object, ignoreName: Boolean): void
    {
        var objectXML: XML = describeType(object);
        for each(var node: XML in xml.attributes())
        {
            if (!ignoreName || node.localName() != "name")
            {
                try
                {
                    var value: *;
                    if (node == "true" || node == "false")
                    {
                        value = (node == "true");
                    }
                    else
                    {
                        value = node;
                    }
                    var setterName: String = node.localName().toString();
                    var type: String = objectXML.accessor.(@name == setterName).@type;
                    if (type == "String")
                    {
                        value = value.toString().replace(quotRegExp, "'");
                        value = value.toString().replace(quotAndExp, "&");
                        value = decodeLocaleTags(value);
                    }
                    object[setterName] = value;
                }
                catch(e: ReferenceError)
                {
                    //trace("copyXmlAttributesToObject: " + e.message);
                }
            }
        }
    }

    private function decodeLocaleTags(value: String): String
    {
        if (value.indexOf("[[") < 0) return value;
        //var regExp: RegExp = new RegExp("\\[\\[(.*)\\]\\]", "g");
        var regExp: RegExp = new RegExp("\\[\\[([^\\]]+)\\]\\]", "g");
        var newValue: String = value;
        while (true) {
            var tag: Array = regExp.exec(value);
            if (tag == null)
                break;
            var tagValue: String = tag[1];//tag.input.substr(2, tag.input.length-4);
            var tagParamList: Array = tagValue.split(",");
            var param0: String = tagParamList[0];
            tagParamList = tagParamList.slice(1);
            var replacedValue: String = Client.instance.str(param0, tagParamList);
						if( replacedValue == null ) replacedValue = param0;
						newValue = newValue.replace(tag[0], replacedValue);
            //newValue = newValue.substring(0, tag.index) + replacedValue + newValue.substring(tag.index+tag.input.length);
        }
        return newValue;
    }

    private function createListFromXML(xml: XML): Array
    {
        var list: ArrayCollection = new ArrayCollection();
        for each(var listNode: XML in xml.children())
        {
            var property: Object = createPropertyFromXml(listNode);
            list.addItem(property);
        }

        return list.source;
    }

}
}