# -*- encoding: utf-8 -*-
##############################################################################
#
#       ______ Releasing children from poverty      _
#      / ____/___  ____ ___  ____  ____ ___________(_)___  ____
#     / /   / __ \/ __ `__ \/ __ \/ __ `/ ___/ ___/ / __ \/ __ \
#    / /___/ /_/ / / / / / / /_/ / /_/ (__  |__  ) / /_/ / / / /
#    \____/\____/_/ /_/ /_/ .___/\__,_/____/____/_/\____/_/ /_/
#                        /_/
#                            in Jesus' name
#
#    Copyright (C) 2014 Compassion CH (http://www.compassion.ch)
#    @author: Emanuel Cino <ecino@compassion.ch>
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU Affero General Public License as
#    published by the Free Software Foundation, either version 3 of the
#    License, or (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU Affero General Public License for more details.
#
#    You should have received a copy of the GNU Affero General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
##############################################################################

{
    "name": "Account Statement Additions for Compassion CH",
    "version": "1.5",
    "author": "Emanuel Cino",
    "category": "Finance",
    "website": "http://www.compassion.ch",
    "depends": ['account_statement_base_completion',
                'crm_compassion', 'hr_expense'
                ],
    "data": ['view/statement_view.xml',
             'view/invoice_view.xml',
             'data/data.xml'],
    "css": ["static/src/css/sheet.css"],
    "description": """

Account Statement Additions for Compassion CH
=========================

- Add three completion methods :
    1. Completion method based on the BVR reference of the contract
       or the invoice.
    2. Completion method based on the reference of the partner
    3. Completion method for Raiffaisen statements (supplier invoices) based
       only on the amount.

- The first rule is applied. If no contract or invoice is found with same
  BVR reference, then second rule is applied, and an invoice is generated
  on-the-fly for gifts or funds donations.
- The third rule is useful only for supplier invoices.

- Adds ability to generate invoices from the bank statement

- Adds field analytic account to move_line view

""",
    "demo": [],
    "test": [],
    "active": False,
    "license": "AGPL-3",
    "installable": True,
    "auto_install": False,
    "application": False,
}
# vim:expandtab:smartindent:tabstop=4:softtabstop=4:shiftwidth=4:
