package com.turbostool.client.net {
import com.turbostool.client.net.messages.*;

import flash.utils.ByteArray;

public class MessageWriter
{
    private function writeBytes(xml: String, bytes: ByteArray): void
    {
        bytes.writeInt(MessageReader.XML_TYPE);
        bytes.writeUTFBytes(xml);
    }

    public function write(event: ServerRequest, bytes: ByteArray): void
    {

        if (event is CarStateDataMessage)
        {
            var csm: CarStateDataMessage = CarStateDataMessage(event);
            bytes.writeInt(MessageReader.BINARY_TYPE);
            bytes.writeFloat(csm.getR().myX);
            bytes.writeFloat(csm.getR().myY);
            bytes.writeFloat(csm.getVelocity().myX);
            bytes.writeFloat(csm.getVelocity().myY);
            bytes.writeFloat(csm.getAngle());
            bytes.writeFloat(csm.getAngularVelocity());
//            bytes.writeFloat(csm.getLateralAngle());
//            bytes.writeFloat(csm.getLateralAngularVelocity());
//            bytes.writeFloat(csm.getLongAngle());
//            bytes.writeFloat(csm.getLongAngularVelocity());
//            bytes.writeFloat(csm.getRudderAngle());
//            bytes.writeInt(csm.getRudderAction());
//            bytes.writeByte(csm.getIsAccelerate() ? 1 : 0);
//            bytes.writeFloat(csm.getEngineDroselCoef());
//            bytes.writeByte(csm.getIsBrake() ? 1 : 0);
//            bytes.writeFloat(csm.getBrakeCoef());
//            bytes.writeByte(csm.getIsHandBrake() ? 1 : 0);
        }
        else
        {
            var xml: String = event.serializeToXML();
            //trace(event.name + ": " + xml);
            writeBytes(xml, bytes);
        }
    }

    public function createBytes(event: ServerRequest): ByteArray
    {
        var bytes: ByteArray = new ByteArray();
        write(event, bytes);
        return bytes;
    }
}
}