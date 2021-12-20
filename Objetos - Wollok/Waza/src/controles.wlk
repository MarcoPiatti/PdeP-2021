import usuarios.*
import vehiculos.*
import date.*

class Control {
    method multar(usuario, costoMulta){
        usuario.serMultado(costoMulta)
    }

    method controlar(usuario, limiteDeVelocidad)
}

class ControlDeVelocidad inherits Control {
    override method controlar(usuario, limiteDeVelocidad){
        if(usuario.vehiculo().velocidad() > limiteDeVelocidad){
            self.multar(usuario, 3000)
        }
    }
}

class ControlEcologico inherits Control {
    override method controlar(usuario, limiteDeVelocidad){
        if(!usuario.vehiculo().esEcologico()){
            self.multar(usuario, 1500)
        }
    }
}

class ControlRegulatorio inherits Control {
    override method controlar(usuario, limiteDeVelocidad){
        const today = new Date.day()
        if(usuario.DNI().even() == today.even()){
            self.multar(usuario, 2000)
        }
    }
}