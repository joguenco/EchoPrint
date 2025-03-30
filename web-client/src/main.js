const server = 'http://localhost:9090'

const ticket = {
  company: 'Resolvedor 10010',
  customer: {
    name: 'Jorge Luis',
    address: 'Street City',
    email: 'jorgeluis@resolvedor.dev',
    phone: '999-999-9999'
  },
  details: [
    { product: 'Web Development', quantity: 1, price: '6.00' },
    { product: 'Desktop Development', quantity: 1, price: '6.00' },
    { product: 'Movil Development', quantity: 1, price: '6.00' }
  ],
  tax: '0.00',
  total: '18.00'
}

let messagePing = document.querySelector('#messagePing')
let messageTicket = document.querySelector('#messageTicket')
let pingButton = document.querySelector('#pingButton')
let payButton = document.querySelector('#payButton')
const modalPing = document.getElementById('modalPing')
const modalTicket = document.getElementById('modalTicket')

pingButton.addEventListener('click', pingToServer)
payButton.addEventListener('click', payTicket)
document.addEventListener('keydown', (event) => {
  if (event.key === "Escape") {
    closeAllModals();
  }
})

const clickClose = function () {
  (document
    .querySelectorAll(
      '.modal-background, .modal-close, .modal-card-head .delete, .modal-card-foot .button'
    ) || []).forEach(($close) => {
      $close.addEventListener('click', () => {
        closeModal(modalPing)
      })
      $close.addEventListener('click', () => {
        closeModal(modalTicket)
      })
    })
}

clickClose()

function pingToServer() {
  const url = `${server}/ping`

  openModal(modalPing)

  fetch(url).then((response) => {
    return response.json()
  }).then((data) => {
    console.log(data)
    messagePing.innerHTML = data.message
  }).catch(function (error) {
    console.log(error)
    messagePing.innerHTML = `Cannot connect to server ${url}`
  })
}

function payTicket() {

  openModal(modalTicket)

  const url = `${server}/print`
  let lines = []

  lines.push({ line: `${ticket.company}` })
  lines.push({ line: `` })
  lines.push({ line: `Contact Info` })
  lines.push({ line: `- - - - - - - - - - - - - - - - - - - - - ` })
  lines.push({ line: `Name: ${ticket.customer.name}` })
  lines.push({ line: `Address: ${ticket.customer.address}` })
  lines.push({ line: `Email: ${ticket.customer.email}` })
  lines.push({ line: `Phone: ${ticket.customer.phone}` })
  lines.push({ line: `- - - - - - - - - - - - - - - - - - - - - ` })
  lines.push({ line: `Product               Qty        Price` })
  lines.push({ line: `- - - - - - - - - - - - - - - - - - - - - ` })
  ticket.details.forEach((detail) => {
    lines.push({ line: `${detail.product} ${detail.quantity} ${detail.price}` })
  })
  lines.push({ line: `- - - - - - - - - - - - - - - - - - - - - ` })
  lines.push({ line: `Tax: ${ticket.tax}` })
  lines.push({ line: `Total: ${ticket.total}` })
  lines.push({ line: `` })
  lines.push({ line: `Thank you for your business!` })

  const data = {
    lines
  }

  fetch(url, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify(data)
  })
    .then(response => {
      if (!response.ok) {
        throw new Error('Network response was not ok')
      }
      return response.json()
    })
    .then(data => {
      console.log('Success:', data)
      messageTicket.innerHTML = data.message
    })
    .catch(error => {
      console.error('Error:', error)
      messageTicket.innerHTML = `Cannot connect to server ${url}`
    })
}

function openModal($el) {
  $el.classList.add('is-active')
}

function closeModal($el) {
  $el.classList.remove('is-active')
}

function closeAllModals() {
  (document.querySelectorAll('.modal') || []).forEach(($modal) => {
    closeModal($modal)
  })
}

let htmlDetail = ''
ticket.details.forEach((detail) => {
  htmlDetail += `
    <tr class="service">
      <td class="tableitem">
        <p class="itemtext">${detail.product}</p>
      </td>
      <td class="tableitem">
        <p class="itemtext">${detail.quantity}</p>
      </td>
      <td class="tableitem">
        <p class="itemtext">$${detail.price}</p>
      </td>
    </tr>
  `
})

document.querySelector('#ticket').innerHTML = `        
          <div id="invoice-POS">
            <center id="top">
              <div class="logo"></div>
              <div class="info">
                <h2>${ticket.company}</h2>
              </div>
              <!--End Info-->
            </center>
            <!--End InvoiceTop-->

            <div id="mid">
              <div class="info">
                <h2>Contact Info</h2>
                <p>
                  Name: ${ticket.customer.name}</br>
                  Address: ${ticket.customer.address}</br>
                  Email: ${ticket.customer.email}</br>
                  Phone: ${ticket.customer.phone}</br>
                </p>
              </div>
            </div>
            <!--End Invoice Mid-->

            <div id="bot">

              <div id="table">
                <table>
                  <tr class="tabletitle">
                    <td class="item">
                      <h2>Product</h2>
                    </td>
                    <td class="Hours">
                      <h2>Qty</h2>
                    </td>
                    <td class="Rate">
                      <h2>Price</h2>
                    </td>
                  </tr>

                  ${htmlDetail}

                  <tr class="tabletitle">
                    <td></td>
                    <td class="Rate">
                      <h2>tax</h2>
                    </td>
                    <td class="payment">
                      <h2>$${ticket.tax}</h2>
                    </td>
                  </tr>

                  <tr class="tabletitle">
                    <td></td>
                    <td class="Rate">
                      <h2>Total</h2>
                    </td>
                    <td class="payment">
                      <h2>$${ticket.total}</h2>
                    </td>
                  </tr>

                </table>
              </div>
              <!--End Table-->

              <div id="legalcopy">
                <p class="legal"><strong>Thank you for your business!</strong> 
                </p>
              </div>

            </div>`