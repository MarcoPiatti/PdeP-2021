import usuarios.*
import zonas.*

object Sistema {
    const usuarios = #{}
    const zonas = #{}

    method pagarMultas() {
        usuarios.forEach({ unUsuario => unUsuario.pagarMultas() })
    }

    method zonaMasTransitada() {
        return zonas.max({ unaZona => unaZona.usuariosEnTransito().size() })
    }

    method usuariosComplicados() {
        return usurios.filter({ unUsuario => unUsuario.multas().sum() > 5000 })
    }
}