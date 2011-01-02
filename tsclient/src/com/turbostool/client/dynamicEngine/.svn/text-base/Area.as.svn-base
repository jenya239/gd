package com.turbostool.client.dynamicEngine
{
import com.turbostool.client.game.route.ObstacleModel;
import com.turbostool.client.utils.collections.ArrayList;
import com.turbostool.client.utils.collections.Collection;

public class Area
{
    private var myObstacles: ArrayList = new ArrayList();

    public function addObstacle(obs: ObstacleModel): void {
        myObstacles.addItem(obs);
    }

    public function deleteObstacle(obs: ObstacleModel): void {
        myObstacles.removeItem(obs);
    }

    public function getObstacles(): Collection {
        return myObstacles;
    }

    //		public function get fuckToString(): String {
    //			return myObstacles.fuckToString;
    //		}

}
}