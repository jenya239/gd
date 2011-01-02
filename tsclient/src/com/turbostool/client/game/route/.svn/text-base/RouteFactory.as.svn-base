package com.turbostool.client.game.route
{
import com.turbostool.client.event.ProgressEvent2;
import com.turbostool.client.event.WorldCreatedEvent;
import com.turbostool.client.game.RaceWorld;
import com.turbostool.client.geom.*;
import com.turbostool.client.newGraphic.*;
import com.turbostool.client.utils.*;
import com.turbostool.client.utils.collections.*;

import flash.display.BitmapData;
import flash.events.*;

public class RouteFactory extends EventDispatcher {
    [Embed(source="/assets/textures/ferrari.png")]
    [Bindable]
    private var ferrariImgCls:Class;
    protected var myCameraPathID:String;
    protected var myStartLineID:String;
    protected var myPreliminaryRaceWorld:RaceWorld;
    protected var myRaceWorld:RaceWorld;
    protected var myAddGraphicCollection:Collection;

    private var myTextureLoader: TextureLoader = new TextureLoader();

    public const DEFAULT_ROAD_WIDTH: Number = 14;

    public static const POLYGON_TAG: String = 'polygon';
    public static const GRAPHICS_TAG: String = 'graphics';
    public static const ITEM_TAG: String = 'item';
    public static const ANGLE_TAG: String = 'angle';
    public static const HEIGHT_TAG: String = 'height';
    public static const LAYER_TAG: String = 'layer';
    public static const ELEMENTS_TAG: String = 'elements';
    public static const ITEM_IS_OBSTACLE:String = 'obstacle'
    public static const TEXTURE_RECT_TAG: String = 'textureRectangle';

    public static const ITEM_TYPE: String = 'type';
    public static const ITEM_NAME: String = 'name';

    public static const ITEM_TYPE_RECT: String = 'rectangle';
    public static const ITEM_TYPE_POLY: String = 'polygon';
    public static const ITEM_TYPE_CONTAINER: String = 'container';

    public static const TEXTURE_REPEAT: String = 'repeat';

    public function RouteFactory() {

    }

    public function getTextureLoader(): TextureLoader {
        return myTextureLoader;
    }

    public function addProgressListener2(listener: Function): void {
        myTextureLoader.addEventListener(ProgressEvent2.PROGRESS_EVENT2, listener);
    }

    public function removeProgressListener2(listener: Function): void {
        myTextureLoader.removeEventListener(ProgressEvent2.PROGRESS_EVENT2, listener);
    }

    public function addProgressListener(listener: Function): void {
        myTextureLoader.addEventListener(ProgressEvent.PROGRESS, listener);
    }

    public function removeProgressListener(listener: Function): void {
        myTextureLoader.removeEventListener(ProgressEvent.PROGRESS, listener);
    }

    protected function getUrlCollection(graphicXML: XML): Collection {
        var items: XMLList = graphicXML.child(ITEM_TAG);
        var result: ArrayList = new ArrayList();
        for (var j: int = 0; j < items.length(); j++) {
            var item: XML = items[j];
            var fileName: String = getFileName(item);
            result.addItem(fileName);
            if (item.hasOwnProperty(ELEMENTS_TAG)) {
                result.addCollection(getUrlCollection(item.child(ELEMENTS_TAG)[0]));
            }
        }
        return result;
    }

    public function initWorldCreation(loaderData: Object, routeName: String): void {
        var routeXml: XML = loaderData as XML;
        myTextureLoader.addEventListener(TextureLoader.ALL_TEXTURES_LOADED, texturesLoadedHandler);
        myTextureLoader.load(getUrlCollection(routeXml.child(GRAPHICS_TAG)[0]));

        function texturesLoadedHandler(e: Event): void {
            myTextureLoader.removeEventListener(TextureLoader.ALL_TEXTURES_LOADED, texturesLoadedHandler);
            createRoad(routeXml.child(POLYGON_TAG)[0], routeName);
            myCameraPathID = routeXml.child(GRAPHICS_TAG).attribute('path');
            myStartLineID = routeXml.child(GRAPHICS_TAG).attribute('start');
            var drawables: Collection = createObjects(routeXml.child(GRAPHICS_TAG)[0]);
            myPreliminaryRaceWorld.addNGDrawables(drawables);
            myRaceWorld = myPreliminaryRaceWorld;
            dispatchEvent(new WorldCreatedEvent(myRaceWorld));
        }
    }


    public function getWorld():RaceWorld {
        if (myRaceWorld == null) {
            throw new TSError("race world has not been created in RouteFactory");
        }
        return myRaceWorld;
    }

    private function getFileName(objectXML: XML): String
    {
        var fileName: String = objectXML.child('filename')[0];
        if (fileName.indexOf("\\") > -1)
        {
            //		    trace("before: " + fileName);
            var allSlashesPattern: RegExp = /\\/g;
            fileName = fileName.replace(allSlashesPattern, "/");
            //		    trace("after : " + fileName);
        }
        return fileName;
    }

    protected function createObject(objectXML: XML): NGDrawable {
        function tryParseNumber(xml: XML, tagName: String, defaultValue: Number = 0): Number {
            if (xml.hasOwnProperty(tagName)) {
                return new Number(xml.child(tagName)[0]);
            } else {
                return defaultValue;
            }
        }

        var type: String = ITEM_TYPE_RECT;
        var result: NGDrawable;
        if (objectXML.attribute(ITEM_TYPE).length() != 0) {
            type = objectXML.attribute(ITEM_TYPE);
        }

        var r: Vector2d = Vector2d.parseTo2dVector(objectXML.child('center')[0]);
        var angle: Number = tryParseNumber(objectXML, ANGLE_TAG);//new Number( objectXML.angle[0] );
        var frame: LocalFrame2d = new LocalFrame2d(r, angle);
        var height: Number = tryParseNumber(objectXML, HEIGHT_TAG);// new Number( objectXML.height[0]);
        var layer: Number = tryParseNumber(objectXML, LAYER_TAG);//new Number( objectXML.layer[0] );

        var fileName: String;
        var psevdoVector: Vector2d;
        var texture: BitmapData;

        if (type == ITEM_TYPE_RECT) {

            psevdoVector = Vector2d.parseTo2dVector(objectXML.child('rectangle')[0]);
            var width: Number = psevdoVector.myX;
            var length: Number = psevdoVector.myY;

            fileName = getFileName(objectXML);

            texture = myTextureLoader.getTexture(fileName);
            result = new NGTexturedRect(frame, height, layer, width, length, texture);
            if (objectXML.attribute('name') == myStartLineID) {
                myPreliminaryRaceWorld.setStartLine((result as NGTexturedRect).getLine());
            }

        } else if (type == ITEM_TYPE_POLY) {

            psevdoVector = Vector2d.parseTo2dVector(objectXML.child('textureRectangle')[0]);
            var textureWidth: Number = psevdoVector.myX;
            var textureLength: Number = psevdoVector.myY;

            fileName = getFileName(objectXML);
            texture = myTextureLoader.getTexture(fileName);

            var poly: Polygon = createPolygon(objectXML.child('polygon')[0]);

            var repeat: Boolean = true;
            if (objectXML.attribute(TEXTURE_REPEAT).length() != 0) {
                repeat = Utils.str2bool(objectXML.attribute(TEXTURE_REPEAT));
            }

            if (objectXML.attribute('obstacle') == "true" && height <= 2) {
                myPreliminaryRaceWorld.addObstacleModel(
                        new ObstacleModel(poly.createGlobal(frame) as Polygon)
                        );
            }
            if (objectXML.attribute('name') == myCameraPathID) {
                myPreliminaryRaceWorld.setCameraPath(poly.createGlobal(frame) as Polygon);
            }
            result = new NGTexturedPolygon(frame, height, layer, poly, texture, textureWidth, textureLength, repeat);
        } else if (type == ITEM_TYPE_CONTAINER) {

            var elements: XML = objectXML.child(ELEMENTS_TAG)[0];
            var drawables: Collection = createObjects(elements);

            result = new NGContainer(frame, height, layer, drawables);
        }
        return result;
    }

    protected function createObjects(graphicXML: XML): Collection {
        var items: XMLList = graphicXML.child(ITEM_TAG);
        var drawables: ArrayList = new ArrayList();
        for (var j:int = 0; j < items.length(); j++) {
            var item: XML = items[j];
            drawables.addItem(createObject(item));
        }
        return drawables;
    }

    private function createPolygon(polygonXML: XML): Polygon {
        var result: Polygon = new Polygon();
        var vertices: XMLList = polygonXML.child('vertex');
        for (var i: int = 0; i < vertices.length(); i++) {
            var v:Vector2d = Vector2d.parseTo2dVector(vertices[i]);
            result.addVertex(v);
        }
        return result;
    }

    public function createRoad(polygonXML: XML, routeName:String):void {

        var list:ArrayList = new ArrayList();
        var path:Polygon;

        var attr: XMLList = polygonXML.attribute('roadWidth');
        var roadWidth: Number = attr.length() == 0 ? DEFAULT_ROAD_WIDTH : new Number(attr);

        path = createPolygon(polygonXML);

        var pol1:Polygon = path.createParallel(- roadWidth / 2);
        pol1.myInsideActive = true;
        var obstacle1:ObstacleModel = new ObstacleModel(pol1);

        var pol2:Polygon = path.createParallel(roadWidth / 2);
        pol2.myInsideActive = false;
        var obstacle2:ObstacleModel = new ObstacleModel(pol2);


        var road:Road = new Road(path, roadWidth);
        list.addItem(road);
        var startLinePiece:Piece2d = new Piece2d(
                pol1.getVertices().getItemAt(0) as Vector2d,
                pol2.getVertices().getItemAt(0) as Vector2d
                );
        var obstacleModels:ArrayList = new ArrayList();
        if (polygonXML.@obstacle == "true") {
            obstacleModels.addItem(obstacle1);
            obstacleModels.addItem(obstacle2);
        }
        var route:RaceWorld = new RaceWorld(list, startLinePiece, path, routeName, obstacleModels);

        //route.addNGDrawable(createStartLineView(startLinePiece));
        myPreliminaryRaceWorld = route;
    }

    private function createStartLineView(startLine: Piece2d): NGDrawable {
        const slLength: Number = 0.6;

        var r:Vector2d = Vector2d.getZero();
        r.addMultiplied(startLine.myBegin, 1 / 2);
        r.addMultiplied(startLine.myEnd, 1 / 2);
        var slFrame: LocalFrame2d = new LocalFrame2d(
                r,
                startLine.getPieceVector().getAngle()
                );
        var slTexture: BitmapData = new ferrariImgCls().bitmapData;
        var slWidth: Number = startLine.getPieceVector().length();
        return new NGTexturedRect(slFrame, 0, LayerConstants.ROAD_MARKUP, slWidth, slLength, slTexture, true);
    }
}

}