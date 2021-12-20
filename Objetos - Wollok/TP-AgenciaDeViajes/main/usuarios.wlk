import localidades.*
import barrileteCosmico.*
import viajes.*
import mediosDeTransporte.*
import perfiles.*

import errores.*

class Usuario {
	const username
	var localidadOrigen
	var perfil
	
	const viajes = []
	const follows = #{}
	var dinero = 0
	
	method viajes() {
		return viajes
	}
	
	method dinero() {
		return dinero
	}
	
	method localidadOrigen() {
		return localidadOrigen
	}

	method puedeViajar (viaje) {
		return dinero >= viaje.costo()
	}
	
	method viajar(destino) {
		const viajesCandidatos = barrileteCosmico.armarViajes(self, destino)
		const nuevoViaje = perfil.elegirViaje(viajesCandidatos, dinero)
		
		if (!self.puedeViajar(nuevoViaje)) throw new NoPuedoViajar(message = "No me alcanza para viajar")
		dinero -= nuevoViaje.costo()
		viajes.add(nuevoViaje)
		localidadOrigen = destino		
	}
	
	method kilometros() {
		return viajes.map({ unViaje => unViaje.distancia()}).sum()
	}
	
	method seguir(unUsuario) {
		if (!self.sigueA(unUsuario)) {
			follows.add(unUsuario)
			unUsuario.seguir(self)
		}
	}
	
	method sigueA(unUsuario) {
		return follows.contains(unUsuario)
	}
}


const vueloIdaALastToninas = new Viaje(origen = goodAirs, destino = lastToninas, transporte = stukaRacuda)
const vueloRetornoAGoodAirs = new Viaje(origen = lastToninas, destino = goodAirs, transporte = stukaRacuda)

const pabloHari = new Usuario(
	perfil = perfilEmpresarial,
	username = "PHari",
	viajes = [vueloIdaALastToninas, vueloRetornoAGoodAirs],
	dinero = 2001500,
	localidadOrigen = goodAirs
)
