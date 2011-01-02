package com.turbostool.client.utils.collections {
import com.turbostool.client.game.components.CarTraceInfo;
import com.turbostool.client.utils.Vector2d;

public class TraceQueueStorage {
    private static const GRID_SIZE: Number = 100;

    private var myGridSize: Number;
    private var myQueues: HashMap = new HashMap();

    public function TraceQueueStorage(gridSize: Number = GRID_SIZE) {
        myGridSize = gridSize;
    }

    public function getQueue(v: Vector2d): TraceQueue {
        var key: String = Math.round(v.myX / myGridSize) + ':' + Math.round(v.myY / myGridSize);
        if (myQueues.containsKey(key)) {
            return myQueues.getValue(key) as TraceQueue;
        } else {
            var queue: TraceQueue = new TraceQueue(CarTraceInfo.TRACES_COUNT * 4);
            myQueues.setValue(key, queue);
            return queue;
        }
    }

    public function clear(): void {
        myQueues.clear();
    }

}
}