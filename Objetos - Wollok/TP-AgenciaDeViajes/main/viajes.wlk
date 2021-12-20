import mediosDeTransporte.*

class Viaje {
    const origen
    const destino
    const transporte
    
    method origen() {
        return origen
    }

    method destino() {
        return destino
    }

    method transporte() {
        return transporte
    }

    method distancia() {
        return origen.distanciaHasta(destino)
    }

    method costoTransporte() {
		return self.distancia() * transporte.costoPorKm()
	}

    method costo() {
        return destino.precio() + self.costoTransporte()
    }
}