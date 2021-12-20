class Producto {
    const nombre
    const precioBase

    method precio() {
        return precioBase * 1.21
    }

    method nombreDeOferta() {
        return "SUPER OFERTA" + nombre
    }
}

class Mueble inherits Producto {
    override method precio() {
        return 1000 + super()
    }
}

class Indumentaria inherits Producto {}

class Ganga inherits Producto {
    override method precio() {
        return 0
    }

    override method nombreDeOferta() {
        return "COMPRAME POR FAVOR" + nombre
    }
}

