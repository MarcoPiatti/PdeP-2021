import cupones.*
import niveles.*
import productos.* 

class CarritoLlenoException inherits Exception {}
class CarritoVacioException inherits Exception {}

class Usuario {
    const nombre
    var dinero = 0
    var puntos = 0
    var nivel = bronce
    const carrito = []
    const cupones = []

    method agregarProductoAlCarrito(producto) {
        if(!nivel.puedeAgregarProducto(carrito)){
            throw new CarritoLlenoException(message = "Usuario no puede agregar mas productos al carrito")
        }
        carrito.add(producto)
    }

    method cuponesUsables() {
        return cupones.filter({cupon => !cupon.fueUsado()})
    }

    method tieneCuponesUsables(){
        return !self.cuponesUsables().isEmpty()
    }

    method tomarCupon() {
        return self.cuponesUsables().anyOne()
    }

    method pagar(costo) {
        dinero -= costo
    }

    method comprarProductosDelCarrito() {
        if(carrito.isEmpty()){
            throw new CarritoVacioException(message = "El carrito esta vacio")
        }
        var costoCompra = carrito.sum({producto => producto.precio()})
        if(self.tieneCuponesUsables()){
            const cupon = self.tomarCupon()
            costoCompra = cupon.aplicar(costoCompra)
        }
        self.pagar(costoCompra)
        puntos += costoCompra * 0.1
        carrito.clear()
    }

    method esMoroso() {
        return dinero < 0
    }

    method perderPuntos(cantidad) {
        puntos = 0.min(puntos-cantidad)
    }

    method eliminarCuponesUsados() {
        cupones.removeAllSuchThat({cupon => cupon.fueUsado()})    
    }

    method actualizarNivel() {
        nivel = niveles.filter({unNivel => unNivel.puedeAcceder(puntos)})
                       .max({unNivel => unNivel.puntosNecesarios()})
    }
}