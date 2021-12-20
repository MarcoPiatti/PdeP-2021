class Cupon {
	var fueUsado = false
	const porcentajeDescuento

	method fueUsado() {
		return fueUsado
	}

	method aplicar(precioBase) {
		fueUsado = true
		return precioBase * porcentajeDescuento
	}
}
