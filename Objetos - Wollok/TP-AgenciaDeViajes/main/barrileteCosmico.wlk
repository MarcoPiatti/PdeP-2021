import localidades.*
import usuarios.*
import mediosDeTransporte.*
import viajes.*

object barrileteCosmico {
	const mediosDeTransporte = #{stukaRacuda, micro, tren, barco}
	const localidades = #{garlicsSea, silversSea, lastToninas, goodAirs,marDelPlata,cerroMalo,machuPichu}
	const usuarios = #{pabloHari}
	
	method mediosDeTransporte() {
		return mediosDeTransporte
	}
	
	method localidades() {
		return localidades
	}
	
	method localidadesMasImportantes() {
		return localidades.filter { unaLocalidad => unaLocalidad.esDestacada() }
	}
	
	method aplicarDescuentos(porcentajeDescuento) {
		localidades.forEach { unaLocalidad => unaLocalidad.aplicarDescuento(porcentajeDescuento) }
	}
	
	method esExtrema() {
		return localidades.any { unaLocalidad => unaLocalidad.esPeligrosa() }
	}
	
	method cartaDeDestinos() {
		return localidades.map { unaLocalidad => unaLocalidad.nombre() }
	}

	method armarViajes(usuario, destino) { 
		const origen = usuario.localidadOrigen()
		const viajesCandidatos = mediosDeTransporte.map({ 
			unTransporte => new Viaje(origen = origen, destino = destino, transporte = unTransporte)
		})
		return viajesCandidatos
	}
}
