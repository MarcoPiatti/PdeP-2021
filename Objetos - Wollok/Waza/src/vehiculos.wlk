class Vehiculo {
    const capacidad
    var combustible
    var velocidad

    method velocidad() {
        return velocidad
    }

    method topeDeCarga(litrosACargar){
        return litrosACargar.min(capacidad-combustible)
    }

    method cargarCombustible(litros){
        combustible += topeDeCarga(litros)
    }

    method esEcologico()

    method recorrer(distancia){
        combustible -= 2
    }
}

class Camioneta inherits Vehiculo {
    override method esEcologico(){
        return false
    }

    override method recorrer(distancia){
        combustible -= 4 + 5 * distancia
    }
}

class Deportivo inherits Vehiculo {
    override method esEcologico(){
        return velocidad < 120
    }

    override method recorrer(distancia){
        super()
        combustible -= 0.2 * velocidad
    }
}

class Familiar inherits Vehiculo {
    override method esEcologico(){
        return true
    }
}