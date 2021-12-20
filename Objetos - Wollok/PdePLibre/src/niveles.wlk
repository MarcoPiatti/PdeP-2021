class Nivel {
    const puntosNecesarios
    
    method puntosNecesarios(){
        return puntosNecesarios
    }

    method puedeAcceder(puntos){
        return puntos >= puntosNecesarios
    }

    method puedeAgregarProducto(carrito)
}

object oro inherits Nivel(puntosNecesarios = 15000){
    override method puedeAgregarProducto(carrito){
        return true;
    }
}

object plata inherits Nivel(puntosNecesarios = 5000){
    override method puedeAgregarProducto(carrito){
        return carrito.size() < 5;
    }
}

object bronce inherits Nivel(puntosNecesarios = 0){
    override method puedeAgregarProducto(carrito){
        return carrito.size() < 1;
    }
}

const niveles = #{bronce, plata, oro}