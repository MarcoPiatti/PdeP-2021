import vehiculos.*

class NoPuedePagarCombustibleException inherits Exception {}

class Usuario {
	const nombre
	const DNI
	var dinero

	var vehiculo

	const multas = []

	method DNI() {
		return DNI
	}

	method vehiculo() {
		return vehiculo
	}

	method multas(){
		return multas
	}

	method puedePagar(gasto){
		dinero >= gasto
	}

	method cargarGasolina(litros){
		gasto = 40 * vehiculo.topeDeCarga(litros)
		if(!self.puedePagar(gasto)){
			throw new NoPuedePagarCombustibleException(
				message = "El Usuario no tiene dinero suficiente para pagar esa cantidad de combustible"
			)
		}
		vehiculo.cargarCombustible(litros)
		dinero -= gasto
	}

	method serMultado(costo){
		multas.add(costo)
	}

	method pagarMultas(){
		multas.forEach({ unaMulta => 
			if(self.puedePagar(unaMulta)){
				multas.remove(unaMulta)
				dinero -= unaMulta
			}
			else unaMulta += unaMulta * 1.1
		})
	}
}
