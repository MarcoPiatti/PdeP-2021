class Localidad {

	const nombre
	const equipajeImprescindible = #{}
	var precio
	const kilometro

	method precio() {
		return precio
	}

	method nombre() {
		return nombre
	}

	method equipajeImprescindible() {
		return equipajeImprescindible
	}

	method kilometro() {
		return kilometro
	}
	
	method esDestacada() {
		return precio > 2000
	}

	method aplicarDescuento(porcentajeDescuento) {
		precio = precio - precio * (porcentajeDescuento / 100)
		self.agregarEquipaje("Certificado de descuento")
	}

	method agregarEquipaje(unEquipaje) {
		equipajeImprescindible.add(unEquipaje)
	}

	method esPeligrosa() {
		return equipajeImprescindible.any{ unEquipaje => unEquipaje.toLowerCase().contains("vacuna") }
	}

	method distanciaHasta(otraLocalidad) {
		return (otraLocalidad.kilometro() - self.kilometro()).abs()
	}

}


class Playa inherits Localidad {
	override method esPeligrosa() {
		return false
	}
}

class Montana inherits Localidad {
	const altura

	override method esPeligrosa() {
		return super() || altura > 5000
	}
	
	override method esDestacada() {
		return true
	}
}

class CiudadHistorica inherits Localidad {
	const cantidadDeMuseos
	
	override method esPeligrosa() {
		return !equipajeImprescindible.any{ unEquipaje => unEquipaje.toLowerCase().contains("seguro de asistencia al viajero") }
	}
	
	override method esDestacada() {
		return super() && cantidadDeMuseos >= 3
	}
}


const garlicsSea = new Localidad(nombre = "Garlic\'s Sea", equipajeImprescindible = #{ "Caña de Pescar", "Piloto" }, precio = 2500, kilometro = 200)

const silversSea = new Localidad(nombre = "Silver\'s Sea", equipajeImprescindible = #{ "Protector Solar", "Equipo de Buceo" }, precio = 1350, kilometro = 400)

const lastToninas = new Localidad(nombre = "Last Toninas", equipajeImprescindible = #{ "Vacuna Gripal", "Vacuna B", "Necronomicon" }, precio = 3500, kilometro = 330)

const goodAirs = new Localidad(nombre = "Good Airs", equipajeImprescindible = #{ "Cerveza", "Protector Solar" }, precio = 1500, kilometro = 0)



const marDelPlata = new Playa(nombre = "marDelPlata", precio = 5000, kilometro = 1600)

const cerroMalo = new Montana(altura = 5001, nombre = "cerro malo", precio = 8000, kilometro = 400)

const machuPichu = new CiudadHistorica(cantidadDeMuseos = 4, nombre = "machu pichu", precio = 4000, kilometro = 2000)
