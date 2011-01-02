package com.turbostool.client.game.components.car
{
import com.turbostool.client.Config;
import com.turbostool.client.utils.Utils;

public class CarLoadRequest
{
    private var _clientID: int;
    private var _carName: String;
    private var _carFileName: String;

    public function CarLoadRequest(carName: String, carFileName:String, clientID: int)
    {
        _clientID = clientID;
        _carName = carName;
        _carFileName = carFileName;
    }

    public function get clientID():int
    {
        return _clientID;
    }

    public function get carName():String
    {
        return _carName;
    }

    public function get carFileName():String {
        return _carFileName;
    }

    public function get texturePath(): String
    {
        var url: String;
        if (carName.indexOf("car_") == 0)
        {
            url = Utils.stringFormat("{0}data/racingCars/r{1}.png", Config.instance.serverUrl, carName);
        } else
        {
            if (Config.initialized)
            {
                url = Utils.stringFormat("{0}data/cars/{1}/{1}.png", Config.instance.serverUrl, carName);
            }
            else
            {
                url = Utils.stringFormat("data/cars/{0}/{0}.png", carName);
            }
        }
        return url;
    }

    public function get carXMLPath(): String
    {
        return Utils.stringFormat("{0}data/cars/{1}/{1}.xml", Config.instance.serverUrl, carFileName);
    }


}
}