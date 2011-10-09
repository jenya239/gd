function try_submit(form, message){
	if ( confirm(message) ){ 
		form.submit();
	}
}

var Tooltip = Class.create({});
Object.extend(Tooltip, {
	box: null,
	init: function(){
		this.box = new Element( 'div', {style: 'position: absolute; left: 0px; top: 0px;'} );
		$(document.body).insert( this.box );
		this.box.hide();
	}
});
Tooltip.addMethods({
	el: null,
	content: '',
	initialize: function( el, content ){
		this.el = el;
		this.content = content;
		if( Tooltip.box == null ) Tooltip.init();
		el.addEventListener('mouseover', function(e){
			Tooltip.box.update( this.content );
			Tooltip.box.style.left = e.pointerX()+'px';
			Tooltip.box.style.top = e.pointerY()+'px';
			Tooltip.box.show();
		}.bind( this ));
		el.addEventListener('mouseout', function(e){
			Tooltip.box.hide();
		}.bind( this ));
	}
});

function create_color_changing( container, car_id, car_class_id, color_id ){
	var changing = new Element( 'span' ).update( color_id );
	container.update( '' );
	container.insert( changing );
	new Ajax.InPlaceEditor( changing, car_color_change_url, {
		size: 3,
		callback: function( form, value ){
			return 'id='+car_id+'&newColor='+parseInt( value );
		},
		onComplete: function( transport, el ){
			var new_color = parseInt( transport.responseText );
			create_color_changing( container, car_id, car_class_id, new_color );
		}
	});
	var pic_url = 'http://188.93.17.91/data/selectCar/car_'+car_class_id+'_'+color_id+'.png';
	var thumb = new Element( 'img', {src: pic_url, height: 13} );
	new Tooltip( thumb, '<img src="' + pic_url + '"/>' );
	container.insert( thumb );
	var rcar_pic_url = 'http://188.93.17.91/data/racingCars/rcar_'+car_class_id+'_'+color_id+'.png';
	var rcar_thumb = new Element( 'img', {src: rcar_pic_url, height: 13} );
	new Tooltip( rcar_thumb, '<img src="' + rcar_pic_url + '"/>' );
	container.insert( rcar_thumb );
}

addEventListener('load', function(){
	$$('.color_changing').each( function( el ){
		var args = el.innerHTML.split(' ');
		create_color_changing( $( el.parentNode ), args[0], args[1], args[2] );
	} );
});