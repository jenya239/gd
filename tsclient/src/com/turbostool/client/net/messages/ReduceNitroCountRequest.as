package com.turbostool.client.net.messages
{
public class ReduceNitroCountRequest extends ServerRequest
{
    public static const REDUCE_NITRO_COUNT: String = "reduceNitroCount";

    public function ReduceNitroCountRequest()
    {
        super(REDUCE_NITRO_COUNT);
    }
}
}