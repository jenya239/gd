package com.turbostool.client.net.messages
{
import com.turbostool.client.model.ClientInfo;
import com.turbostool.client.model.UpgradeInfo;

public class AddClientMessage extends ChanneledMessage
{
    public static const ADD_CLIENT: String = "addClient";

    public var _initial: Boolean = false;
    private var _clientInfo: ClientInfo;
    private var _upgradeInfo: UpgradeInfo;

    public function AddClientMessage(channel: String = "")
    {
        super(channel);
        //_clientInfo = new ClientInfo(_clientID, _displayName, _carID, _loadingProgress);
    }

    public function get clientInfo(): ClientInfo
    {
        return _clientInfo;
    }

    public function set clientInfo(value:ClientInfo):void
    {
        _clientInfo = value;
        if (_upgradeInfo != null) {
            _clientInfo.upgradeInfo = _upgradeInfo;
        }
    }

    public function get upgradeInfo(): UpgradeInfo
    {
        return _upgradeInfo;
    }

    public function set upgradeInfo(value:UpgradeInfo):void
    {
        _upgradeInfo = value;
        _clientInfo.upgradeInfo = _upgradeInfo;
    }

    public function get initial():Boolean
    {
        return _initial;
    }

    public function set initial(value:Boolean):void
    {
        _initial = value;
    }
}
}
