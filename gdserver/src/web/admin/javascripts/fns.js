function try_submit(form, message){
	if ( confirm(message) ){ 
		form.submit();
	}
}