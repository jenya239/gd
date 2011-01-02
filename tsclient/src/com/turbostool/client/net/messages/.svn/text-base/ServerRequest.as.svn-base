package com.turbostool.client.net.messages
{
import flash.utils.describeType;
import mx.utils.StringUtil;

public class ServerRequest
{
    private var _name: String;

    public function ServerRequest(name: String)
    {
        _name = name;
    }

    [Serializable(order=0)]
    public function get name(): String
    {
        return _name;
    }

    public function serializeToXML(): String
    {
        var resultXML: String = '<event ' + serializeBody() + '/>';
        return resultXML;
    }

    protected function serializeBody(): String
    {
        var result: String = "";
        var objectXML: XML = describeType(this);
        var xmlList: XMLList = objectXML.accessor.(elements("metadata").@name == "Serializable");

        var a: Array = new Array();
        for each(var node: XML in xmlList)
        {
            var o: Object = new Object();
            o.name = node.@name;
            o.order = node.metadata.(@name == "Serializable").arg.(@key == "order").@value;
            o.escape = node.metadata.(@name == "Serializable").arg.(@key == "escape").@value;
            a.push(o);
        }

        a = a.sortOn("order");

        for each(var p: Object in a)
        {
            var value: String = "";
            if(p.escape == "true") {
                var xml: XML = new XML("<a />");
                xml.@b = this[p.name];
                var s: String = xml.toXMLString();

                value = s.substring(6, s.length - 3);

            } else {
                value = this[p.name];
            }
            
            result += p.name + '="' + value + '" ';
        }

        return result;
    }

    public function toString(): String
    {
        return "[" + name + "]";
    }
}
}