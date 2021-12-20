class MedioDeTransporte {
    const demora
	
    method demora() {
        return demora
    }

    method costoPorKm()
}

class Avion inherits MedioDeTransporte {
	const turbinas = []
	
	override method costoPorKm() {
		return turbinas.map({ unaTurbina => unaTurbina.impulso() }).sum()
	}
}

class Turbina {
	const impulso
	method impulso() {
		return impulso
	}
}

class Micro inherits MedioDeTransporte {
	override method costoPorKm() {
		return 5000
	}
}

class Tren inherits MedioDeTransporte { 
	const costoPormilla = 2300
								
	method convertirCostoAKM() {
		return costoPormilla / 1.609
	}
	
	override method costoPorKm() {
		return self.convertirCostoAKM()
	}
}

class Barco inherits MedioDeTransporte { 
	const probabilidadChocarIceberg
	
	override method costoPorKm() {
		return 1000 * probabilidadChocarIceberg
	}
}

const stukaRacuda = new Avion(turbinas = [new Turbina(impulso = 1000)], demora = 5)

const micro = new Micro(demora = 4)

const tren = new Tren(demora = 10)

const barco = new Barco(demora = 14, probabilidadChocarIceberg = 0.5)
