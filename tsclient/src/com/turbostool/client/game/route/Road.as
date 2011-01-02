package com.turbostool.client.game.route
{
import com.turbostool.client.game.IGameComponent;
import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.geom.Polygon;
import com.turbostool.client.newGraphic.LayerConstants;
import com.turbostool.client.newGraphic.NGContainer;
import com.turbostool.client.newGraphic.NGDrawable;
import com.turbostool.client.utils.Ort2d;
import com.turbostool.client.utils.Vector2d;
import com.turbostool.client.utils.collections.ArrayList;
import com.turbostool.client.utils.collections.Collection;
import com.turbostool.client.utils.collections.Iterator;

import flash.utils.describeType;

public class Road implements IGameComponent {
    private static const SLIDING: Number = 1.1;
    private static const ROLLING: Number = 0.01;
    private static const STATIC: Number = 1.2;

    private static var instanceCount:int = 0;

    private var myRoadView: NGContainer;
    private var myName:String;

    public function Road(path: Polygon, roadWidth: Number) {
        instanceCount++;
        myName = getClassName() + instanceCount;

        var pol1:Polygon = path.createParallel(- roadWidth / 2);
        pol1.myInsideActive = true;
        var pol2:Polygon = path.createParallel(roadWidth / 2);
        pol2.myInsideActive = false;

        var it1:Iterator = pol1.vertexIterator();
        var it2:Iterator = pol2.vertexIterator();
        var v1Prev: Vector2d, v2Prev: Vector2d;
        var v1: Vector2d, v2: Vector2d;

        if (it1.hasNext() && it2.hasNext()) {
            v1Prev = pol1.getVertices().lastElement() as Vector2d;
            v2Prev = pol2.getVertices().lastElement() as Vector2d;
        }

        var ngRoadPieces: ArrayList = new ArrayList();
        while (it1.hasNext() && it2.hasNext()) {
            v1 = it1.next() as Vector2d;
            v2 = it2.next() as Vector2d;
            var poly: Polygon = new Polygon();
            poly.addVertex(v1Prev);
            poly.addVertex(v1);
            poly.addVertex(v2);
            poly.addVertex(v2Prev);
            //var dir : Ort2d = v1.difference2d(v1Prev).ort2d();
            //ngRoadPieces.addItem(new NGRoadPiece(poly, dir, roadWidth));
            v1Prev = v1;
            v2Prev = v2;
        }

        myRoadView = new NGContainer(new LocalFrame2d(new Vector2d(0, 0), 0), 0, LayerConstants.ROAD, ngRoadPieces);
    }

    public function getName():String {
        return myName;
    }


    public function getClassName():String {
        var fullName:String = describeType(this).attribute("name").toString();
        return fullName.split('::')[1];
    }

    public function getDrawables():Collection {
        return new ArrayList(myRoadView);
    }

    public function getModels():Collection {
        return new ArrayList();
    }

    public function getNGDrawable(): NGDrawable {
        return myRoadView;
    }
}
}